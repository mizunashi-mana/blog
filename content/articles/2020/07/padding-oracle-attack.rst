CBC mode に対しての Padding Oracle Attack
=========================================

:tags: ブロック暗号, 脆弱性
:category: セキュリティ

共通鍵暗号の暗号方式としてよく用いられている暗号の種類として，ブロック暗号がある．ブロック暗号は，暗号方式の大別で，固定長のデータを単位として処理するような暗号の総称である．ところで，もちろん暗号化の対象となるデータは，固定長とは限らないし，かなり長さが大きくなる場合もある．そこで，ブロック暗号を扱うデータサイズより長いデータに対しても利用できるよう補佐する暗号利用モードと呼ばれるメカニズムも用意されている．

ところで，この暗号利用モードは，誤って利用すると，元となったブロック暗号が優秀であろうと致命的に安全性が損なわれる場合が多いことが知られている．今回はそのケースのうち，CBC と呼ばれる暗号利用モードについて知られている攻撃手法，padding oracle attack の概要を見ていく．

CBC (Cipher Block Chaining)
---------------------------

ブロック暗号として，

* 暗号化アルゴリズム :math:`E_K: \Sigma^n \to \Sigma^n`
* 複合アルゴリズム :math:`D_K: \Sigma^n \to \Sigma^n`

があったとする．ここで，:math:`\Sigma^n` は入出力として考えられる長さ :math:`n` の文字列の集合，:math:`K` は鍵である．これを任意の長さの文字列 :math:`P \in \Sigma^*` に適用することを考える．つまり，

* 暗号化アルゴリズム :math:`E^+_K: \Sigma^* \to \Sigma^*`
* 複合アルゴリズム :math:`D^+_K: \Sigma^* \to \Sigma^*`

を作りたい．さて，状況を簡単にするため，入出力の長さがブロック長 :math:`n` の倍数になっている状況を考える．この時，入出力は長さ :math:`n` の文字列 (ブロック) の列と考えられる．つまり，

* 暗号化アルゴリズム :math:`E^+_K: (\Sigma^n)^* \to (\Sigma^n)^*`
* 複合アルゴリズム :math:`D^+_K: (\Sigma^n)^* \to (\Sigma^n)^*`

が作れれば，その列を平坦にしたものを入出力の文字列として捉えることで，ブロック暗号を拡張できる．で，その拡張方法だが，単純に思いつくのは以下のようなものだ:

* :math:`E^+_K = \{P_i\}_{i \in [m]} \mapsto \{E_K(P_i)\}_{i \in [m]}`
* :math:`D^+_K = \{C_i\}_{i \in [m]} \mapsto \{D_K(C_i)\}_{i \in [m]}`

つまり，単純に各ブロックにそのまま元のブロック暗号を適用する方法だ．この方法は ECB モードと呼ばれている．ただ，この方法は

* 同じブロックは同じブロックに暗号化されるため，暗号文を見て同じブロックの箇所が判定できる
* ブロック単位で複製・削除・順序入れ替えなどの改竄ができる

と言った問題がある．そのため，もう少し改良を加えたい．ECB の問題点は，主にそれぞれのブロックが独立に扱われていることに由来する．そこで，それぞれのブロックに対して他のブロックの内容に依存するようなベクトルを注入することが考えられる．CBC モードは，この注入を XOR で行うようなモードになる．

CBC モードは，初期ベクトル :math:`\mathit{Iv}` が与えられた時，

.. math::

  E^+_K = \{P_i\}_{i \in [m]} \mapsto \{C_i\}_{i \in \{0\} \cup [m]}

  C_i = \left\{\begin{array}{ll}
    \mathit{Iv} &(i = 0) \\
    E_K(P_i \oplus C_{i - 1}) &(\text{otherwise})
  \end{array}\right.

.. math::

  D^+_K = \{C_i\}_{i \in \{0\} \cup [m]} \mapsto \{D_K(C_i) \oplus C_{i - 1}\}_{i \in [m]}

というような拡張を施す．一応よくある図を，NIST の資料から拝借して掲載しておく [#nist-block-cipher-modes]_:

.. image:: {attach}padding-oracle-attack/cbc-image.png
  :alt: CBC モードのイメージ図
  :align: center

最初は初期ベクトルが注入され，その後は一つ前のブロックの暗号文がベクトルとして注入されるようになる．これにより，EBC と異なり各ブロックはそれ以前のブロックの内容に依存するようになる．さて，このモードの正当性は，以下のように確かめられる:

.. math::

  \begin{array}{ll}
  D^+_K(E^+_K(\{P_i\}_{i \in [m]}))
  &= \{D_K(E_K(P_i \oplus C_{i - 1}) \oplus C_{i - 1}\}_{i \in [m]} \\
  &= \{P_i \oplus C_{i - 1} \oplus C_{i - 1}\}_{i \in [m]} \\
  &= \{P_i \oplus (C_{i - 1} \oplus C_{i - 1})\}_{i \in [m]} \\
  &= \{P_i \oplus 0\}_{i \in [m]} \\
  &= \{P_i\}_{i \in [m]}
  \end{array}

このように，最終的に暗号化と復号で注入したベクトルが XOR によりうまく打ち消し合い，最終的に平文に戻る．これが CBC モードの概要になる．

ところで，CBC モードを利用するには，入力の長さがブロック長の倍数になっていないといけなかった．ブロック長は AES では 128bit，つまり 16byte になる．つまり，AES を CBC モードで利用するには，入力のバイト数が 16 の倍数である必要がある．これは不便なので，パディングを埋め込む運用が通常取られている．

よく用いられている方法が，PKCS#7 由来の方法 [#rfc-5652]_ で，足りないバイト数分そのバイト数に相当する整数を埋めると言う方法だ．今回の例で言えば，例えば

::

  30 31 32 33 34 35 36 37 38 39 40 41

というような 12 byte のデータに対して，パディングを埋めたい場合，足りないバイト数は 4 バイトなので，

::

  30 31 32 33 34 35 36 37 38 39 40 41 42 04 04 04 04

というように 4 byte 分 4 を埋める．ところで，パディングを埋め込む場合，元データとパディングを区別できる必要がある．ただ，バイト数が 16 の倍数になっている場合パディングを埋め込まないようにすると，パディングが埋め込まれた結果のデータなのか，それともパディングが何も埋め込まれていない場合のデータなのかが分からない．そこで，パディングを埋め込む必要がない場合，つまりバイト数が 16 の倍数になっている場合も新しくパディングのみで構成されたブロックを追加するようにする．例えば，

::

  30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46

というようなデータの場合は，

::

  30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10

のように 16 (16進数表記で 10) を 16byte 分末尾に足す．これにより，全てのデータは必ずパディングが埋め込まれており，データの末尾を見れば何バイト分がパディングか分かるようになるため，パディングと元データを区別することができるようになる．

CBC モードを使用する場合は，上記のようなパディングを埋め込んでバイト数をブロック長の倍数にし，それから暗号化を行う．また，復号した後はパディングを取り除くことで多くの場合運用されている．

Padding Oracle Attack
---------------------

このパディングによる運用をつくことで，CBC モードにより暗号化された文を解読する攻撃が，padding oracle attack である．

CBC では，平文 :math:`\{P_i\}_{i \in [m]}`，その暗号文 :math:`\{C_i\}_{i \in \{0\} \cup [m]}` について，以下の関係が成り立つ:

.. math::

  D_K(C_i) \oplus C' = D_K(E_K(P_i \oplus C_{i - 1})) \oplus C' = P_i \oplus C_{i - 1} \oplus C'

ここで，

.. math::

  P' = D_K(C_i) \oplus C'

とおくと，平文 :math:`P_i` に対して，

.. math::

  P_i = P' \oplus C_{i - 1} \oplus C'

という関係が求まる．つまり，何かブロック :math:`C'` があった時，それに対応する :math:`P'` が求まれば，:math:`i - 1` 番目の暗号ブロックから :math:`i` 番目の平文を復元することが可能になる．もちろん，鍵 :math:`K` とアルゴリズム :math:`D` が分かっていれば，:math:`P'` はそのまま求まる．しかし，攻撃側は普通鍵を知らないため，この攻撃を成功させるには :math:`P'` を何らかの方法で求める必要がある．ところで，もし攻撃対象のアプリケーションが複合時パディング情報が合っているかを検証し，わざわざ検証結果を教えてくれる場合，実は :math:`P'` を求めることができる．padding oracle attack の名前の由来は，攻撃対象が padding oracle，つまり padding が合っているかをわざわざ教えてくれる機能を持つ場合に，攻撃できる手法ということだ．

さて，では具体的にどうするかだが，この攻撃では :math:`P'` を求める式が復号処理とよく似ていることに着目する．つまり，復号機に :math:`C' C_i` を送ってみると，復号機はそれをちゃんとした暗号文だと思い :math:`D_K(C_i) \oplus C'` を計算するはずだ．これは :math:`P'` と同じになる．ところで，先ほどの :math:`P_i` を復元する式では，:math:`C'` は :math:`P'` が関係を守っているならば何でも良かった．そこで，この攻撃では最終的に :math:`P'` が，

::

  10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10

つまり，パディングのみになるような :math:`C'` を探す．この探索は最後の 1byte から順に探していく．

今回は，攻撃対象はわざわざパディングが合っているかを検証しその結果を教えてくれるという機能，padding oracle を持っていた．そこで，:math:`C'` を中身は適当に設定し，最後の 1byte だけ順に 0 から 255 まで変えながら，padding oracle に送りつける．この時，padding oracle がパディングが正当であるという結果を返してくるということは，

* たまたまパディングが正当な :math:`C'` を設定できた
* :math:`P'` の最後が :math:`01` になった

の2択になる．ここでは，後者になるよううまく :math:`C'` を調整できていると仮定する．さて，この :math:`C'` を使うと，実は最後の byte を自由にいじることができる．例えば，:math:`C'` の最後のバイト :math:`C'_{16}` に対して :math:`C'_{16} \oplus 01 \oplus 02` としたものを新たに :math:`C''` とおくと，

.. math::

  D_K(C_i)_{16} \oplus C''_{16} = D_K(C_i)_{16} \oplus C'_{16} \oplus 01 \oplus 02 = (01 \oplus 01) \oplus 02 = 02

となる．後は，これにより最後が ``02`` となる :math:`P'`，:math:`C'` が得られるので，その :math:`C'` をベースにまた最後から 2byte 目を変遷させて padding oracle に送りつけ，パディングが正当になる，つまり :math:`P'` の最後が ``02 02`` になるような :math:`C'` を探す．これを順に最初のバイトまで繰り返すと，最終的に :math:`P'` が

::

  10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10

となるような :math:`C'` が得られることになる．後は，:math:`C'` との関係を守る :math:`P'` が得られたので，それを使って平文を復元すればいい．

実際に攻撃してみる
------------------

さて，ところで padding oracle をわざわざ提供してくれるアプリケーションは存在するんだろうか？ ここでは，「わざわざ」とわざとらしく言っているが，実際には意識せず padding oracle を提供してしまう場合がある．多くの場合，サーバの返すエラーの種類が複数ある場合だ．多くの言語が提供する暗号化インターフェースでは，padding の不整合はそれ専用のエラーが出される．例えば Ruby では，

::

  bad decrypt (OpenSSL::Cipher::CipherError)

というようなエラーになる．Java では専用の例外 `BadPaddingException <https://docs.oracle.com/en/java/javase/14/docs/api/java.base/javax/crypto/BadPaddingException.html>`_ が用意されていて，こいつが投げられる．これらをもし，復号された後の文に対するバリデーションとは異なる例外ハンドリングで処理し，結果を異なる形 (例えばエラーメッセージをそのまま返すなど) でユーザに表示してしまうと，padding oracle ができてしまうことになる．

では，実際に padding oracle のあるサービスに対して攻撃をしてみる．今回は簡易的に，Ruby の以下のメソッドに対して攻撃を行う:

.. code-block:: ruby

  require 'openssl'

  $n = 16

  def encrypt(data)
      enc = OpenSSL::Cipher.new("AES-256-CBC")
      enc.encrypt

      enc.key = 'secret key'.ljust(32)
      iv = 'CBC IV'.ljust($n)
      enc.iv = iv

      iv + enc.update(data) + enc.final
  end

  def decrypt(data)
      sleep(0.01)

      dec = OpenSSL::Cipher.new("AES-256-CBC")
      dec.decrypt

      dec.key = 'secret key'.ljust(32)
      dec.iv = data[0, $n]

      dec.update(data[$n, data.length]) + dec.final
  end

  def valid_padding?(data)
      begin
          decrypt(data)
          true
      rescue OpenSSL::Cipher::CipherError
          false
      end
  end

攻撃側が使用できるのは，``valid_padding?`` メソッドのみである．気分を出すため，``decrypt`` は 0.01 秒のレイテンシが出るようにしているが，これはまあ本質的ではないので無視してもらって良い．では，実際に攻撃側を実装していく:

.. code-block:: ruby

  def attack(data)
      cs = data.scan(/[\s\S]{#{$n}}/)
      ps = []

      (cs.length - 1).downto(1) do |i|
          pre_c = cs[i - 1]
          c = cs[i]
          plain = '?' * $n
          c_dash = "\x00" * $n

          is_illegal = true

          puts "Start detecting #{i}th block..."
          1.upto($n) do |j|
              0.upto(255) do |k|
                  c_dash[-j] = k.chr

                  print '.' if k % 10 == 0
                  if valid_padding?(c_dash + c)
                      plain[-j] = (j ^ c_dash.bytes[-j] ^ pre_c.bytes[-j]).chr
                      c_dash = c_dash.bytes.map { |x| x ^ j ^ (j + 1) }.pack('C*')

                      is_illegal = false

                      puts <<~EOS
                      detected #{$n - j}th byte!
                      P: #{plain.inspect}
                      C': #{c_dash.inspect}
                      EOS

                      break
                  end
              end
          end

          if is_illegal
              raise "Something illegal happend!"
          end

          ps.unshift(plain)
      end

      ps.join
  end

基本的には，確認用出力以外は上記で述べたアルゴリズムをそのまま実装するだけだ．ただ平文の復元は，1byte ごとの :math:`C'` の値が分かればそれぞれのバイトごとに復元可能なので，見やすいよう 1byte ごとに復元している．これで，

.. code-block:: ruby

  plain_data = "This is a sample text to crypt by AES-CBC."
  cipher_data = encrypt(plain_data)

  p cipher_data
  p attack(cipher_data)

というようなプログラムを走らせれば，最終的に

::

  "CBC IV          St\xD7\xF0V\xADf\x05\xDB\x04\xAE\xE7/-\xF8\xC8\x91\xCFFL\xD8\xD8\xDD\x0F\xD3\xBC\b\xCBa]ZO\xAA\x06@F;g\x14\x81s\xF4_\x0E\x85\x15_\x1A"
  Start detecting 3th block...
  ........detected 15th byte!
  P: "???????????????\u0006"
  C': "\x03\x03\x03\x03\x03\x03\x03\x03\x03\x03\x03\x03\x03\x03\x03K"
  ..........detected 14th byte!
  P: "??????????????\u0006\u0006"
  ...
  ....detected 1th byte!
  P: "?his is a sample"
  C': "\x11::Cy/C\x10Q\x10CQ]@\\U"
  .detected 0th byte!
  P: "This is a sample"
  C': "\x06;;Bx.B\x11P\x11BP\\A]T"
  "This is a sample text to crypt by AES-CBC.\u0006\u0006\u0006\u0006\u0006\u0006"

というような出力が得られ，攻撃が成功することが確かめられる．

Encryption Attack
-----------------

ここまでは暗号文の解読に焦点を当ててきたが，同様の方法で改竄も可能だ．まず，パディングがちゃんと付けられた平文 :math:`\{P_i\}_{i \in [m]}` があるとして，これを暗号化して送りつけたいとする．攻撃側は鍵を知らないので，これを他の方法でちゃんとした暗号文にする必要がある．

さて，平文に余分に :math:`P_{m + 1}` を付け加えた :math:`\{P_i\}_{i \in [m + 1]}` を考える．そして，そのちゃんとした暗号文を :math:`\{C_i\}_{i \in \{0\} \cup [m + 1]}` とおいた時，前の

.. math::

  P' = D_K(C_i) \oplus C'

を満たす :math:`P'`，:math:`C'` が見つかれば，

.. math::

  C_{i - 1} = P_i \oplus P' \oplus C'

という関係が成り立つことになる．:math:`P'`，:math:`C'` は上記の padding oracle attack の手法をそのまま使えば求められ，これによりある平文のブロックからその前のブロックに対応する暗号文が見つけられることになる．最終的に :math:`C_{m + 1}` 以外の暗号ブロックを求めることができ，:math:`C_{m + 1}` は余分に付け加えたブロックに対応する暗号文なので，それを取り除けば元の平文に対する暗号文が得られる．

実際に実装してみると，

.. code-block:: ruby

  def encrypt_attack(data)
      pad_len = $n - data.length % $n
      pad_len = $n if pad_len == 0

      ps = (data + pad_len.chr * pad_len)
        .scan(/[\s\S]{#{$n}}/)

      ps.push("\x00" * $n)
      cs = ["\x00" * $n]

      (ps.length - 1).downto(0) do |i|
          plain = ps[i]
          c = cs[0]
          pre_c = "?" * $n
          c_dash = "\x00" * $n

          is_illegal = true

          puts "Start detecting #{i}th block..."
          1.upto($n) do |j|
              0.upto(255) do |k|
                  c_dash[-j] = k.chr

                  print '.' if k % 10 == 0
                  if valid_padding?(c_dash + c)
                      pre_c[-j] = (plain.bytes[-j] ^ j ^ c_dash.bytes[-j]).chr
                      c_dash = c_dash.bytes.map { |x| x ^ j ^ (j + 1) }.pack('C*')

                      is_illegal = false

                      puts <<~EOS
                      detected #{$n - j}th byte!
                      C: #{pre_c.inspect}
                      C': #{c_dash.inspect}
                      EOS

                      break
                  end
              end
          end

          if is_illegal
              raise "Something illegal happend!"
          end

          cs.unshift(pre_c)
      end

      cs[0, cs.length - 1].join
  end

のようになる．実際に

.. code-block:: ruby

  plain_data = "This is a sample text to crypt by AES-CBC."

  attacked_cipher_data = encrypt_attack(plain_data)
  p attacked_cipher_data
  p decrypt(attacked_cipher_data)

のように攻撃してみると，

::

  Start detecting 3th block...
  ...................detected 15th byte!
  C: "???????????????\xBD"
  C': "\x03\x03\x03\x03\x03\x03\x03\x03\x03\x03\x03\x03\x03\x03\x03\xBF"
  ....................detected 14th byte!
  C: "??????????????\xC0\xBD"
  C': "\x02\x02\x02\x02\x02\x02\x02\x02\x02\x02\x02\x02\x02\x02\xC3\xBE"
  ...
  ..........detected 1th byte!
  C: "?8\x80jG\xC0\xD8\xBF\x18[X@\xB1\xE7T\x93"
  C': "\x11@\xF9\tw\xB9\xBB\x8Fik;1\xCC\x87(\xE6"
  ..................detected 0th byte!
  C: "\xF48\x80jG\xC0\xD8\xBF\x18[X@\xB1\xE7T\x93"
  C': "\xB1A\xF8\bv\xB8\xBA\x8Ehj:0\xCD\x86)\xE7"
  "\xF48\x80jG\xC0\xD8\xBF\x18[X@\xB1\xE7T\x93\xC2[\xE2~\xC8\xAEO\x9Cf\x11}\x8F\xA5X\xC3\xED\xAE\eRI\x06Uk\x9E\xCB\x02n\xF9l\xC2E\x1A\xC8\xE8\x83\xBBc\x97\x82\x90R\xD5N]4\xFA\xC0\xBD"
  "This is a sample text to crypt by AES-CBC."

のような出力が得られ，攻撃が成功してることが確認できる．

まとめ
------

というわけで，padding oracle attack の概要を見た．padding oracle を利用することで，暗号文からの平文の復元，改竄を行えることが分かった．ユーザに対して情報を提供することはユーザビリティの面では非常に重要だが，セキュリティの面では少しの情報で致命的な欠陥を招くこともある．そういうのは気をつけていかなきゃなと思いました（こなみ）．

なお，以下の文献に非常にお世話になった:

* `Padding Oracle AttackによるCBC modeの暗号文解読と改ざん - security etc... <http://rintaro.hateblo.jp/entry/2017/12/31/174327>`_
* `CBC modeに対するPadding oracle attackをやってみる - ももいろテクノロジー <http://inaz2.hatenablog.com/entry/2015/12/23/000923>`_

.. [#nist-block-cipher-modes] https://csrc.nist.gov/publications/detail/sp/800-38a/final
.. [#rfc-5652] http://tools.ietf.org/html/rfc5652#section-6.3
