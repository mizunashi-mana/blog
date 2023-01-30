SecureRandom.getInstanceStrong を気軽に使ってはいけない
===========================================================

:tags: Java, 乱数, JVM, Linux
:category: セキュリティ

何番煎じか分からないが、個人的に詰まったので備忘録。JDK には、暗号用の乱数生成器 `SecureRandom <https://docs.oracle.com/javase/jp/11/docs/api/java.base/java/security/SecureRandom.html>`_ クラスが用意されている。そして、このクラスには、 `getInstanceStrong <https://docs.oracle.com/javase/jp/11/docs/api/java.base/java/security/SecureRandom.html#getInstanceStrong()>`_ というメソッドが用意されている。この名前はいかにも強そうで、基本的にはこのメソッドを使っていれば安全そうに見える。ところが、このメソッドは何も考えずに使うと思わぬ落とし穴にハマることになる。今回はその話をする。

突然フリーズするサーバ
-------------------------

アリスはボブに頼まれ、単純なデータベースへの読み書きを担当する API サーバを作ることになった。読み書きするデータは機密性の高いもので、万が一にも漏洩するのは良くないし、生データに触れるのも制限された人のみにすべきで、それはアリスに対しても開示できないものなので、データベースに保存するデータは全てサーバ側で書き込み側から鍵を受け取って暗号化してから保存することになった。アリスは普段使い慣れている Java でサーバを書くことにした。開発は順調で、手元でアリスは万全のテストを行い、自信を持ってボブに完成したサーバプログラムを渡した。

ところがその数日後、ボブからクレームが来ることになる。君からもらったプログラムはまるで使いものにならない、サーバはある程度リクエストを受け付けるとフリーズしてしまうというのだ。それも、秒間数千リクエストとかではない。ほんの数リクエスト程でというのだ。それぐらいの状況ならアリスはテスト済みだ。どうせサーバの起動環境の問題だろうと思いつつ、ボブに請われて原因調査のためインスタンスに潜る。すると確かに、アリスが作ったサーバはほんの数リクエストでレスポンスを返さなくなってしまうようだった。ただ、データベースへの接続が止まっている雰囲気でもない。不思議に思いながら、アリスはひとまずスレッドダンプを取ってみることにした。 ``jps`` でサーバのプロセス ID を調べ、 ``jstack`` でスレッドダンプを表示してみたところ、何やら不思議なダンプ情報が出力された::

    $ jstack 9
    ...
    "Thread-0" #13 prio=5 os_prio=0 cpu=23.44ms elapsed=12.94s tid=0x0000ffff88172de0 nid=0x1c runnable  [0x0000ffff5cdec000]
    java.lang.Thread.State: RUNNABLE
            at java.io.FileInputStream.readBytes(java.base@17.0.6/Native Method)
            at java.io.FileInputStream.read(java.base@17.0.6/FileInputStream.java:276)
            at java.io.FilterInputStream.read(java.base@17.0.6/FilterInputStream.java:132)
            at sun.security.provider.NativePRNG$RandomIO.readFully(java.base@17.0.6/NativePRNG.java:425)
            at sun.security.provider.NativePRNG$RandomIO.getMixRandom(java.base@17.0.6/NativePRNG.java:405)
            - locked <0x000000008cf91998> (a java.lang.Object)
            at sun.security.provider.NativePRNG$RandomIO.implNextBytes(java.base@17.0.6/NativePRNG.java:537)
            at sun.security.provider.NativePRNG$Blocking.engineNextBytes(java.base@17.0.6/NativePRNG.java:269)
            at java.security.SecureRandom.nextBytes(java.base@17.0.6/SecureRandom.java:758)
            at DataCryptor.encrypt(DataCryptor.java:66)
            at ApiService.registerData(ApiService.java:15)
            at Server.run(Server.java:10)
            at java.lang.Thread.run(java.base@17.0.6/Thread.java:833)
    ...

どうやら、 ``SecureRandom.nextBytes`` の呼び出しが止まっているようだ。該当部分のコードは以下のようになる:

.. code-block:: java

    ...

    public class DataCryptor {
        ...

        private final ThreadLocal<Cipher> cipherThreadLocal = new ThreadLocal<>() {
            protected Cipher initialValue() {
                try {
                    return Cipher.getInstance(CIPHER_ALGORITHM);
                } catch (NoSuchAlgorithmException | NoSuchPaddingException e) {
                    throw new RuntimeException("unreachable: expect AES/GCM/NoPadding support", e);
                }
            }
        };

        private final ThreadLocal<byte[]> gcmNonceBytesThreadLocal = new ThreadLocal<>() {
            protected byte[] initialValue() {
                return new byte[12];
            }
        };

        private final SecretKeySpec keySpec;
        private final SecureRandom random;

        DataCryptor(byte[] key) {
            assert key.length * 8 == KEY_BIT_LENGTH;

            this.keySpec = new SecretKeySpec(key, KEY_ALGORITHM);
            try {
                this.random = SecureRandom.getInstanceStrong();
            } catch (NoSuchAlgorithmException e) {
                throw new RuntimeException("unreachable: expect SecureRandom strong algorithm support", e);
            }
        }

        public byte[] encrypt(byte[] plain) {
            final Cipher cipher = cipherThreadLocal.get();
            final byte[] nonce = gcmNonceBytesThreadLocal.get();
            this.random.nextBytes(nonce);

            ...
        }

        ...
    }

これの ``encrypt`` メソッド3行目が該当部分のようだった。なんの変哲もない ``SecureRandom.nextBytes`` の呼び出しに見える。一体全体なぜこんなところでフリーズするんだろう。もう一度ダンプをよく見てみよう::

            at sun.security.provider.NativePRNG$RandomIO.implNextBytes(java.base@17.0.6/NativePRNG.java:537)
            at sun.security.provider.NativePRNG$Blocking.engineNextBytes(java.base@17.0.6/NativePRNG.java:269)
            at java.security.SecureRandom.nextBytes(java.base@17.0.6/SecureRandom.java:758)

どうやら、 ``SecureRandom.nextBytes`` の呼び出しは実際には ``NativePRNG.Blocking.engineNextBytes`` と呼び出しになるようだ。名前に ``Blocking`` という文字が入っているのが気になるところだ。さて、そもそもこの記事はタイトル落ちなのだが、実際の問題の箇所は以下の部分だ:

.. code-block:: java

        DataCryptor(byte[] key) {
            assert key.length * 8 == KEY_BIT_LENGTH;

            this.keySpec = new SecretKeySpec(key, KEY_ALGORITHM);
            try {
                this.random = SecureRandom.getInstanceStrong();
            } catch (NoSuchAlgorithmException e) {
                throw new RuntimeException("unreachable: expect SecureRandom strong algorithm support", e);
            }
        }

つまり、 ``SecureRandom`` のインスタンスを作成してる部分、つまり ``this.random = SecureRandom.getInstanceStrong()`` の部分だ。ただ、この原因の説明には少し時間が必要だ。では、なぜこのような現象が起こるのか見ていこう。

SecureRandom.getInstanceStrong
------------------------------------

アリスがつまづいたポイントを理解するには、まず ``SecureRandom.getInstanceStrong`` メソッドがどういう動作をするかを理解しなければいけないだろう。`Oracle の JavaSE 19 向けのドキュメント <https://docs.oracle.com/javase/jp/19/docs/api/java.base/java/security/SecureRandom.html#getInstanceStrong()>`_ には、以下のような記述がある:

    securerandom.strongAlgorithmsSecurityプロパティで指定されたアルゴリズムまたはプロバイダを使用して選択されたSecureRandomオブジェクトを返します。

    RSA公開/非公開キーのような付加価値の高い永続的な秘密情報を作成する場合など、状況によっては強力な乱数値が必要になります。 アプリケーションが適切で強力なSecureRandom実装を選択できるようにするため、Javaディストリビューションでは、securerandom.strongAlgorithmsセキュリティ・プロパティに既知の強力なSecureRandom実装のリストが含まれています。

つまり、 ``SecureRandom.getInstanceStrong`` メソッドが返すインスタンスはSecurityプロパティの ``securerandom.strongAlgorithms`` の値によって変わり、そのプロパティも Java ディストリビューションによって変わるというわけだ。Securityプロパティというのは、`Security.getProperty <https://docs.oracle.com/javase/jp/19/docs/api/java.base/java/security/Security.html#getProperty(java.lang.String)>`_ で取得できる値のこと。`java.security.Security のドキュメント <https://docs.oracle.com/javase/jp/19/docs/api/java.base/java/security/Security.html>`_ によると

    セキュリティ・プロパティのデフォルト値は、実装固有のロケーション(通常はJavaインストール・ディレクトリのプロパティ・ファイルconf/security/java.security)から読み取られます。

とのことで、`OpenJDK の定義 <https://github.com/openjdk/jdk/blob/jdk-19%2B36/src/java.base/share/conf/security/java.security#L168-L173>`_ では、以下のようになっている:

Windows 以外
    NativePRNGBlocking:SUN,DRBG:SUN

Windows
    Windows-PRNG:SunMSCAPI,DRBG:SUN

この形式についての説明はちょっと見つけられなかったが、`OpenJDK の実装 <https://github.com/openjdk/jdk/blob/jdk-19%2B36/src/java.base/share/classes/java/security/SecureRandom.java#L933-L977>`_ を読む限り、 ``Algorithm1:Provider1,Algorithm2:Provider2,...`` という形式のようだ。優先順位は左からのようだ。つまり、Windows 以外の環境では NativePRNGBlocking というアルゴリズムが SUN プロバイダにあればそれが使われる。Windows では Windows-PRNG というアルゴリズムが SunMSCAPI プロバイダにあればそれが使われる。そして、それらが無ければ DRBG というアルゴリズムが SUN プロバイダにあればそれが使われるということになる。

アリスの手元は macOS/AArch64、サーバが実際に動いた環境は Linux/x64 の環境だった。そしてどちらの環境も SUN プロバイダで NativePRNGBlocking アルゴリズムが提供されていた。つまり、どちらにしても ``SecureRandom.getInstanceStrong()`` は ``SecureRandom.getInstance("NativePRNGBlocking", "SUN")`` と同じということで、アルゴリズムの違いによって動作に違いが生まれていたわけではない。問題なのはこの NativePRNGBlocking アルゴリズム自体の動作というわけだ。

SecureRandom のアルゴリズム
---------------------------------

さて、NativePRNGBlocking の詳細に入る前に、SecureRandomの実装アルゴリズムの概要について少し見ておこう。アルゴリズムの一覧は、 `Javaセキュリティ標準アルゴリズム名 <https://docs.oracle.com/javase/jp/19/docs/specs/security/standard-names.html#securerandom-number-generation-algorithms>`_ 及び `Java SE セキュリティ開発者ガイド <https://docs.oracle.com/javase/jp/19/security/oracle-providers.html#GUID-9DC4ADD5-6D01-4B2E-9E85-B88E3BEE7453>`_ で説明されている。それぞれまとめると

+-----------------------+------------+--------------------------------------------------------------------------+
| アルゴリズム          | プロバイダ | 概要                                                                     |
+=======================+============+==========================================================================+
| NativePRNG            | SUN        | 基盤となるネイティブOSから乱数を取得する。 乱数生成のブロック性について  |
|                       |            | は何も表明されない。基本実装は、``nextBytes`` では ``/dev/urandom`` 、   |
|                       |            | ``generateSeed`` では ``/dev/random`` が使用される。                     |
+-----------------------+------------+--------------------------------------------------------------------------+
| NativePRNGBlocking    | SUN        | 基盤となるネイティブOSから乱数を取得し、必要に応じてブロック化する。基本 |
|                       |            | 実装は、``nextBytes`` 、 ``generateSeed`` で ``/dev/random`` が使用され  |
|                       |            | る。                                                                     |
+-----------------------+------------+--------------------------------------------------------------------------+
| NativePRNGNonBlocking | SUN        | 基盤となるネイティブOSから乱数を取得するが、アプリケーションの速度低下を |
|                       |            | 避けるためブロックしない。基本実装は、``nextBytes`` 、 ``generateSeed``  |
|                       |            | で ``/dev/urandom`` が使用される。                                       |
+-----------------------+------------+--------------------------------------------------------------------------+
| PKCS11                | SunPKCS11  | 基礎となるインストール済および構成済のPKCS #11ライブラリから乱数を取得す |
|                       |            | る。                                                                     |
+-----------------------+------------+--------------------------------------------------------------------------+
| DRBG                  | SUN        | NIST SP 800-90Ar1 で定義されている DRBG メカニズムを使用するアルゴリズ   |
|                       |            | ムを使用する。                                                           |
+-----------------------+------------+--------------------------------------------------------------------------+
| SHA1PRNG              | SUN        | 各操作につき値が1増加する64ビット・カウンタを使って鎖状につながった真に  |
|                       |            | ランダムなシード値から、SHA-1ハッシュを計算する疑似乱数生成アルゴリズム  |
|                       |            | を使用する。初期シードは、現在、システム属性とjava.securityエントロピー  |
|                       |            | 収集デバイスの組合せによって決定される。                                 |
+-----------------------+------------+--------------------------------------------------------------------------+
| Windows-PRNG          | SunMSCAPI  | 基盤となるWindows OSから乱数を取得する。                                 |
+-----------------------+------------+--------------------------------------------------------------------------+

ここまでで何が問題か分かった人は多いと思うが、 ``NativePRNGBlocking`` の必要に応じてブロックするというところ、特に ``/dev/random`` が通常使われるというところが問題になる。簡単に言えば ``/dev/random`` つまり ``NativePRNGBlocking`` の取得元は真の乱数のみを生成するようになっており、十分な乱数性が保証されていない状態では乱数性が保証されるまでブロックを行うのだ。Linux での乱数生成についてはこの後触れるが、基本的に OS は普段からハードウェア割り込みなどタイミングが予測不能のイベントを観測するなどして環境ノイズから乱数源を確保している。そして、 ``/dev/random`` が参照されるとその乱数源に溜め込まれたノイズから乱数を生成する。ただ、ノイズは使ったら再利用できないので、乱数生成ごとに使えるノイズは減っていくわけで、無限にノイズを貯めるのも難しいため枯渇する状況は発生してしまう。そのような場合に ``/dev/random`` はノイズがまた必要な分貯まるまで乱数生成をブロックするというわけだ。環境ノイズは基本的に稼働している外部デバイスがある程度多くないとあまり貯まらない。特に、サーバ用途で普段外部要因があまり関わらないような放置された環境だと、ノイズ元があまりなく、十分な乱数源が確保されるまで時間がかかることが多い。例え、乱数生成がブロックされなかったとしても、本当に真の乱数が必要な他のプログラムに影響を与えるということにもなりかねない。ここから、サーバなどで ``/dev/random``、つまり ``NativePRNGBlocking`` アルゴリズムを使う場合は注意が必要だ。

さて、今回のアリスのサーバは意図して ``/dev/random`` を使ったわけではないが、 ``SecureRandom.getInstanceStrong()`` の中身が実際には ``NativePRNGBlocking`` だったため、結果的にノイズを消費しつつノイズがなくなったら貯まるまでブロックするという動作を起こすコードになっていたというわけだ。手元では、普段からキーボード入力やマウス入力、音声デバイスの使用など様々なノイズ源があるため、あまり ``/dev/random`` のブロックという事態までには至りにくかったが、ノイズが枯渇しがちな実際の環境ではデータ暗号化毎にノイズを消耗してしまうため、ノイズの枯渇という事態に陥りサーバがフリーズするという事態になったわけだ。アリスは結局、 ``SecureRandom.getInstanceStrong()`` を使う代わりにブロックしないことが保証されている ``SecureRandom.getInstance("NativePRNGNonBlocking")`` を使うことにした。その後サーバのフリーズという事態は起きず、ボブの事業も成長していきましたとさ。めでたしめでたし。

ところで、アリスはブロックしないことを理由に ``NativePRNGNonBlocking`` を使うことにしたわけだが、この選択は正しいのだろうか？そもそもなぜ ``SecureRandom.getInstanceStrong`` はデフォルトでブロックするような実装になるのだろうか？ 先ほど少し触れたが、基本的に ``NativePRNGBlocking`` がブロックを起こすのは結果的な話で、目的としては真の乱数のみを使えるようにすることにある。では、 ``NativePRNGNonBlocking``、つまり ``/dev/urandom`` とはどういう実装になっているのかという話なのだが、こちらは暗号的な強度をある程度落とす代わりにブロックしないようなアルゴリズムになっているのだ。具体的には、初期シードはノイズ源から生成しそこから疑似乱数を生成するような生成器を用意しておき、ノイズ元が不足している場合はそちらの生成器から乱数を生成するようになっているのだ。そんなことをして安全なんだろうか？ そこら辺の詳細は後で説明するが、使用上の注意点は確かに幾つかある。ただ、基本的に ``/dev/urandom`` も速度より安全性を重視した実装になっており、注意点が守られるなら長期に何度も再利用されるような乱数列を生成するのでない限りは目的に沿うぐらいの強度は得られるだろう。

実際、 ``SecureRandom`` のコンストラクタでインスタンスを作った場合、標準的には ``NativePRNG`` が実装として使われることになるが、この際 ``nextBytes`` で使われるのも ``/dev/urandom`` になる。基本的には、 ``SecureRandom.getInstanceStrong`` を使う場面というのは JavaSE のドキュメントで述べられている通り、付加価値が高く永続性も高いような乱数列の生成、つまり暗号化用の秘密鍵のようなものだ。それ以外についても、性能が許すなら真の乱数源からの生成を行なってもいいだろうが、あまりにノイズを消費しすぎると、他のプログラムにも影響が出かねないし、ノイズの質も落ちやすくなってしまう。そこのトレードオフを考えつつ、使用するべきだろう。なお、 ``SecureRandom`` の標準的な実装の優先順位、つまりコンストラクタでインスタンスを作った場合に選ばれるアルゴリズムの優先順位は `セキュリティ開発者ガイド <https://docs.oracle.com/javase/jp/19/security/oracle-providers.html#GUID-9DC4ADD5-6D01-4B2E-9E85-B88E3BEE7453>`_ で述べられており、以下のようになっている:

Linux
    1. NativePRNG
    2. DRBG
    3. SHA1PRNG
    4. NativePRNGBlocking
    5. NativePRNGNonBlocking

macOS
    1. NativePRNG
    2. DRBG
    3. SHA1PRNG
    4. NativePRNGBlocking
    5. NativePRNGNonBlocking

Windows
    1. DRBG
    2. SHA1PRNG
    3. Windows-PRNG

なお、Linux、macOS には注記があり、

    java.securityのエントロピー収集デバイスをfile:/dev/urandomまたはfile:/dev/randomに設定すると、SHA1PRNGよりNativePRNGが優先されます。それ以外の場合は、SHA1PRNGが優先されます。

ということのようだ。OpenJDK では基本このプロパティが設定されているようなので、優先順位は基本書いた通りになると思っていいだろう。先ほどのアルゴリズムの解説を合わせると、どのプラットフォームでも優先度が高いものは、シード生成は真の乱数を元に生成、乱数生成はシードを元にした疑似乱数の生成ということになり、 ``/dev/urandom`` 相当のものになることが分かる。アリスの場合シード生成には ``SecureRandom`` インスタンスを使っていないため、標準基準と同レベルと言えるもので、特に問題ないだろう。

なお余談だが、アリスのスレッドダンプは https://github.com/mizunashi-mana/securerandom-getinstancestrong-freeze-cfp のリポジトリのコードで再現した。基本的には、 ``/dev/random`` をパイプファイルで上書きしておき、 ``/dev/random`` のブロックを擬似的に再現している。興味があったら覗いてみてくれ。

OS の乱数生成
-------------------

さて、 ``SecureRandom`` のインスタンス選びには多少注意が必要なことは分かってきたと思う。最後に、 ``SecureRandom`` 標準のアルゴリズムが内部で使用している、 ``/dev/random``、 ``/dev/urandom`` についてもう少し中身を詳しく見ておこうと思う。なお、以降の話は Linux カーネルでの話。それ以外は知らん。

Linux の ``/dev/random``、 ``/dev/urandom`` の実装は最近結構大きく変わった [#linux-rng-history]_。ただまだ変更後のカーネルを使ってるところは少ないだろうと思うので、一応変わる前後について両方見ておこう。混乱を避けるため、変更前のアーキテクチャを Linux-Legacy-RNG、変更後のアーキテクチャを ChaCha20-DRNG と呼ぶことにする。まずは Linux-Legacy-RNG の方から見ていく。さて、今まで ``/dev/random`` は OS が集めた環境ノイズを消費しつつ乱数を生成するというぼかした説明をしてきた。正確には、乱数の生成源はいくつか種類があり、その概要は以下のようになっている:

.. image:: {attach}use-securerandom-getinstancestrong-carefully/linux-rng-overview.png
  :alt: Linux-Legacy-RNG の概要図
  :align: center

なお、この図は以下のレポートから拝借した:

    Patrick Lacharme, Andrea Röck, Vincent Strubel, and Marion Videau. 2012. The Linux Pseudorandom Number Generator Revisited. Retrieved from https://eprint.iacr.org/2012/251.pdf

まず、乱数の生成源の乱雑さを表す状態量をエントロピーと呼ぶが、OS はこのエントロピーをエントロピー元、つまりハードウェア割り込みなどの環境ノイズから上手く抽出し、入力プール (input pool) に溜め込んでおく。このエントロピーが高いほど、入力プールは質の良い状態、つまり極めて乱雑になっているということだ。入力プールは実際には、固定サイズのビット列プールで、エントロピー源から得たビット情報を都度混ぜ合わせた乱数列を保持している。ただ、 ``/dev/random``、 ``/dev/urandom`` は直接この入力プールからエントロピーを抽出して使うのではなく、一度ビット情報を必要に応じてそれぞれのプールに持ってくる。 ``/dev/random`` はブロッキングプールを参照する。ブロッキングプールは自身のエントロピーを監視しつつエントロピーが不足してくると都度入力プールからエントロピーを抽出し、自身の今の情報と混ぜ合わせながらエントロピーを補充、つまり乱雑なビット列を保つ。そして、 ``/dev/random`` の参照元にその乱雑なビットを返しつつ参照された分自身へのフィードバックをしつつ、エントロピーの評価を下げていく。 ``/dev/urandom`` が参照する非ブロッキングプールも基本的には同じようなことを行うが、エントロピーが入力プールから補充できない場合は補充を諦め、現在のプールの状態のまま乱数列を生成する。まあ、ここら辺の具体的なアルゴリズムは興味があれば実装を読んでみるといいと思うが、実益を兼ねてということであれば、この後紹介する ChaCha20-DRNG の方を参照することをお勧めする。Linux-Legacy-RNG のコードベースはかなり混沌としており、この状況の改善も ChaCha20-DRNG プロジェクトの一つの目的であり、そっちの方が読みやすい。また、今後は ChaCha20-DRNG の方が主流になっていくだろうからだ。

さて、ChaCha20-DRNG では Linux-Legacy-RNG と異なり、ブロッキングプールが実は廃止された。以下がその概要図になる:

.. image:: {attach}use-securerandom-getinstancestrong-carefully/chacha20-drng-overview.png
  :alt: ChaCha20-DRNG の概要図
  :align: center

なお、この図は以下のレポートから拝借した:

    2020. Documentation and Analysis of the Linux Random Number Generator. Retrieved from https://www.bsi.bund.de/SharedDocs/Downloads/EN/BSI/Publications/Studies/LinuxRNG/LinuxRNG_EN_V4_3.pdf

さて、もちろんこの背景にはいくつか説明が必要だろう。まず、最近の Linux は ``/dev/random`` を参照しても真の乱数を生成しない。もちろん真の乱数とは何かという議論はあるだろうが、ここで言いたいことはつまり、エントロピー源から得られた乱雑さだけをその都度使うことは無くなったということだ。言い換えれば、 ``/dev/random`` はこれまでの ``/dev/urandom`` と同じく、エントロピー源からシードを生成したり、フィードバックを受けたりすることはあるが、基本的にはブロックせず疑似乱数によって生成されるようになったということだ。これを聞くと Linux-Legacy-RNG の話はなんだったのかと多くの人は思うだろう。この背景は割と複雑で、政治的な事情も色々孕んでいるのだが、ひとまずそこは置いておいて、ひとまず ChaCha20-DRNG の概要を見ていこう。

ChaCha20-DRNG でも、Linux-Legacy-RNG と同じようにエントロピーを貯める入力プールがある。ただ、Linux-Regecy-RNG の時のようなブロッキングプール、非ブロッキングプールといったプールは存在せず、 ``/dev/random``、 ``/dev/urandom`` は共通の一つの状態を参照する。状態は固定長のビット列領域で、ChaCha20 の乱数ストリーム生成に使用される。状態ははじめエントロピープールから生成されたシードで初期化され、その後は入力プールが更新されたかによらず入力プールの内容と状態のデータを使って状態を更新し続けながら、乱数生成をしていく。もちろん、入力プールに環境ノイズが入ればそれは都度反映されるし、環境ノイズが入らなくても暗号学的に安全な乱数生成が行われることになる。これにより、 ``/dev/random`` と ``/dev/urandom`` の違いは、一番最初のシード初期化が完了するまで待つか、それを待たずに警告のみでエントロピーの反映が甘いかもしれない乱数を警告込みでブロックしないで生成するかの違いになった。シード初期化自体も基本的には1秒ほどの短時間で終わるため、ほとんどの場合 ``/dev/random`` と ``/dev/urandom`` の違いを意識して使用する必要は無くなったわけだ。

さてこのような変更が受け入れ可能なら、なぜ Linux-Legacy-RNG では2つのデバイスの使い分けが必要だったのだろうか？ まずこのような変更が可能になったのには、一つには乱数生成器が大きく改善されたというのがある。Linux-Legacy-RNG の時代は SHA1 実装だったのだが、ChaCha20-DRNG では BLAKE2 による実装になっており、攻撃耐性が色々向上している [#linux-random-improvement]_。この強化により、エントロピーをそこまで過剰に反映させなくても、初期化時に十分に反映されていればある程度安全性の高い乱数が生成されるようになったというのがある。もう一つは、そもそもブロッキングプールに対する暗号学的耐性への疑念があるということだ。これは結構昔から言われていたことなのだが、真の乱数を使えば攻撃耐性が備えられるというのは誤りであり、品質の向上に繋がるかも怪しい面が多いというのがある。環境ノイズはそもそも環境によって左右されるわけだが、逆に言えば環境が突然激変しない限り規則性を生む場合も多い。また、時間帯などで品質が大きく変わってくる。実際、環境ノイズだけを乱数の情報源として使用してしまうと、逆に攻撃耐性が下がるというのは割と知られており、むしろ環境ノイズを使うなら疑似乱数や状態をちょいちょい混合してやらないといけないというのが昨今のベストプラクティスで、 ``/dev/random`` と ``/dev/urandom`` の差が実際に安全性に大きい差があるかについては懐疑的な声も多かった。どちらもエントロピーの反映自体は行われているわけで、違いは単純にその量だけであり、SHA1 LFSR 実装ならばともかく、そこから脱却した今となってはその違いは大きいものではないということだ。また、 ``/dev/random`` がブロックすることに至っては DoS 攻撃の要因にもなり兼ねないため、セキュリティ的に負の面もあった。このようなリスクに対して真面目に対処してまで、 ``/dev/random`` にエントロピーを確実に反映させる必要があるのかという不満は Linux 開発者の間で割と上がってはおり、ChaCha20-DRNG ではその声が反映されたという形になる。将来的には、 ``/dev/random`` と ``/dev/urandom`` の中身を全く同じにする、 ``/dev/urandom`` を廃止するという動きもある。もし、アリスが案件を引き受けたのが5年後だったら、もしかしたら今回の問題とは無縁でいられたかもしれない。

というわけで、最近の Linux ではあまり必要性がなくなってきているが、一応入力プールの状態の監視方法についてもまとめておこうと思う。入力プールの状態を監視する為、proc ファイルがいくつか用意されている。それぞれ以下のようになる:

``/proc/sys/kernel/random/poolsize``
    入力プールのビットサイズを表す。

``/proc/sys/kernel/random/entropy_avail``
    入力プールのエントロピービットサイズを表す。これが ``/proc/sys/kernel/random/poolsize`` と同じならエントロピーは十分にあるということで、 ``0`` だとエントロピーが枯渇していることを表す。ChaCha20-DRNG では初期化後は基本 ``/proc/sys/kernel/random/poolsize`` と同じになる。

他にもいくつかあるが、基本はこの2つを見ておけばいいだろう。node exporter にはこいつらの collector がデフォルトで有効になっていて、それぞれ ``node_entropy_pool_size_bits``、 ``node_entropy_available_bits`` で見れる。この辺を監視していれば、アリスやボブももう少し早く原因に気づけたかもしれない。

まとめ
---------------

というわけで今回は Java の ``SecureRandom.getInstanceStrong()`` を使う際の注意点について紹介した。 ``SecureRandom.getInstanceStrong()`` は Linux 環境では基本的には ``/dev/random`` を参照する実装になり、  ``/dev/random`` はエントロピーが不足するとブロックする挙動になるため注意が必要だ。また、付加価値が高く永続性が高い機密情報でもない限りデフォルトの ``new SecureRandom()`` のインスタンスを使うので十分であり、そちらはブロックを行わないため問題が起きにくい。 ``SecureRandom.getInstanceStrong()`` を単に名前が強そうだからという理由で使うのではなく、自身のサービスの特性に合うアルゴリズムをちゃんと選ぶのが重要だろうということだった。最も最近の Linux では事情が異なり、 ``/dev/random`` と ``/dev/urandom`` の違いがなくなりつつあるため、今後はその違いについて考慮する必要はなくなってくるだろう。

``/dev/random`` を使ってはいけないみたいな話時々聞くが実際にどういう話なのか実は知らなかったので、今回のはためになった。最も、この知識がこれから役立つことはあまりなくなっていきそうだが。では、今回はこれで。

.. [#linux-rng-history] 詳しい経緯は https://lwn.net/Articles/884875/ を参照するのが良いだろう。
.. [#linux-random-improvement] https://www.zx2c4.com/projects/linux-rng-5.17-5.18/inside-linux-kernel-rng-presentation-sept-13-2022.pdf
