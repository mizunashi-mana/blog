Ansible の変数を CUE で記述する
=============================================

:tags: Ansible, YAML, CUE, データ検証
:category: 運用

Ansible や Kubernetes などのオーケストレーションツールでは、タスクやサービスの記述は再利用可能にしておきつつ、設定を別途記述することである程度カスタマイズできるようにすることがよくある。さらに、設定ファイルは環境毎に一部設定を変えたり、一部を使い回したりと、柔軟に組み合わせて管理がしたいことがある。Ansible や Kubernetes ではそれぞれ専用の機能が用意されている。しかしながら、これらの機能はそこまで設定の組み合わせについてのサポートが手厚いわけではなく、また設定に対してのスキーマ管理などもないため設定が自らの手で管理しきれなくなることが多い。つまり、設定の更新忘れや記述漏れが発生したり、しばらく触らないでいるとどこをどういじればいいか分からなくなるのだ。その結果、デプロイ作業が恣意的なものになりがちで、手元でテストでしても本番でミスるとかいうことにつながりがちだ。

そこで僕は最近、設定ファイルを CUE で記述しマージはそっち任せにして、そこから生成したものを使うということをやってる。今回は、Ansible の設定管理を題材にその方法を紹介する。

Ansible と変数
-------------------------------

Ansible 使ってる人には説明の必要もない気がするが、一応前提の共有から。Ansible では、構成の宣言によってデプロイタスクを定義して実行する。この構成の宣言を部分的にグループ化する機能も、Ansible では幾つか提供されている。例えば、Ansible では変数を Jinja テンプレートで使用することで、外部から注入された変数によってタスクを調整することができる。さらに、そういう変数をインベントリグループ毎に設定し、マージする機能も提供している。

例えば、

.. code-block:: yaml

    prod:
        children:
            public_prod:
                hosts:
                    public-prod001.private:

というようなインベントリを記述すると、 ``public-prod001.private`` というホストが ``all``、 ``prod``、 ``public_prod`` というグループに所属することになる。このグループそれぞれで変数を設定でき、

1. ``public_prod``
2. ``prod``
3. ``all``

の優先順位で、変数が統合されて使われる。これらの変数は、 ``group_vars/`` というディレクトリに、 ``<グループ名>.yml`` でおけば自動で読み取られる。例えば、 ``group_vars/all.yml`` に

.. code-block:: yaml

    app_version: '1.0.0-rc'
    env: local

    worker_process: 100
    resolver: '8.8.8.8'

``group_vars/prod.yml`` に

.. code-block:: yaml

    app_version: '1.0.0'
    env: production

``group_vars/public_prod.yml`` に

.. code-block:: yaml

    worker_process: 1024

    public_domain: example.com

などと記述した場合、 ``public-prod001.private`` のデプロイ時には、

.. code-block:: yaml

    app_version: '1.0.0'
    env: production

    worker_process: 1024
    resolver: '8.8.8.8'

    public_domain: example.com

という変数群が使用されることになる。この機能を使用することで、開発環境と本番環境で異なる設定を部分的に使用する場合は環境毎にグループを作ってそこの変数で設定、アプリケーション毎に異なる設定を部分的に使用する場合はアプリケーション毎にグループを作ってそこで設定と言ったことができる。

この機能はシンプルで、多くの需要を満たせる反面、ミスに繋がりやすく、またあまりにグループ構成を複雑化しすぎると変数管理が手に負えなくなる面もある。あまり複雑なグループ構成を採用するなと言う話ではあるのだろうが、以下の場面においてより柔軟な設定管理をしたい時はあるだろう:

* 一部のアプリケーションで共通の設定を使いたい。
* ある設定と別の設定を連動させたい。
* あるタスクの設定を、一部は環境毎に、一部はアプリケーション毎に設定したい。

このような時に、上記のグループ機能を純粋に使うと、かなりグループ構成が複雑になりメンテを楽にするための共通化などがメンテの足を引っ張ることになる時がある。つまり、あるタスクの設定をどこに書き、それがどのホストに対して使われる設定なのか把握するのがかなり困難になるのだ。このような状況だと大規模なリファクタリングなど怖くて行えない。またマージ機能はその性質上、デフォルト値を与えつつ必要な場合は明示的に指定を書くと言ったパターンが頻出するが、その時にあるホストに対して最終的にどの値が使われるかが把握しにくい。

もう一つの課題が、どういう変数が設定ができるかの管理方法だ。Ansible では変数はタスクやテンプレートなどの随所で使用されるわけだが、そうなるとどの変数はどういう値を設定すべきで、どの設定は必須なのかみたいな情報を探すのは割と一苦労で取りこぼしなども出てきてしまう。そういうのを一元的に確認したいことがよくあるのだ。ドキュメントに残すと言うのは一つの方法だが、何の検証もされないドキュメントはタイポなどのミスを生みがちで、また陳腐化もしやすく、ドキュメントを参照しても一元的な確認ができないと言う本末転倒な事態を招きかねない。

僕は一時期これらの課題に対して、JSON schema で設定できる変数をまとめ陳腐化を防ぎつつ、タスクの書き方を工夫することで設定の分離もなんとか管理できるようにして、凌いでいた。ただこれも結構課題があり、特に JSON schema がマージとの相性が悪いこと、マージ機能が貧弱なためにタスクを複雑化させるのは割と本末転倒感があることなどから、代替策を探していた。最近はこれらの課題は大体 CUE 言語使えば解決できることが分かってきたので、CUE で書くようにしている。

CUE 言語のモデル
-------------------------

`CUE 言語 <https://cuelang.org/>`_ は、公式サイトではデータ検証言語と紹介されている。基本的には、YAML や JSON などと同じデータ記述が想定された言語なのだが、データ検証を記述できること、データのマージについて標準的なサポートがあること、豊富なテンプレート記述ができることなどが異なる点だ。特徴的なのが、データ検証とマージに関する機能で、型、制約と値の間に束をモデルとした順序を設け、その順序による階層に違反しないかの検証をデータ検証として採用しつつ、マージもその階層に沿って行う。これにより、柔軟にデータを分割しつつ、結合的で交換的で冪等で安全なデータの結合を実現する。まあ、言葉で説明しても想像しにくいと思うので、実際の例を見ていく。

CUE 言語を試すだけなら、オンラインで `試せる <https://cuelang.org/play/>`_。手元で試したい場合は、https://cuelang.org/docs/install/ を見ながら ``cue`` コマンドを入れ、CUE ファイルを作り ``cue eval --all <file>`` で CUE ファイルの評価ができる。例えば、

.. code-block:: cue

    sample_string: string
    sample_string: =~"^[a-z]{3}_[a-z]{2}$"
    sample_string: "abc_de"

は、

.. code-block:: cue

    sample_string: "abc_de"

に評価される。 ``string`` は文字列型、 ``=~"^[a-z]{3}_[a-z]{2}$"`` は正規表現制約、 ``"abc_de"`` は文字列リテラルになる。これらの間には順序関係があり、 ``string`` より ``=~"^[a-z]{3}_[a-z]{2}$"`` の方が大きく、 ``=~"^[a-z]{3}_[a-z]{2}$"`` より ``"abc_de"`` の方が大きい。これらの中で一番大きいのが ``"abc_de"`` なため、これが ``sample_string`` の値として採用されている。CUE 言語の値のマージにおいては、値間の順序だけが大事なので、以下も同じ結果になる:

.. code-block:: cue

    sample_string: =~"^[a-z]{3}_[a-z]{2}$"
    sample_string: "abc_de"
    sample_string: string
    sample_string: "abc_de"

つまり、どの宣言を先に持ってきても良いし、同じ宣言が複数出てきても良い。これが交換的で冪等であると言うことだ。また、一部を別の変数に括り出すことも可能だ。例えば

.. code-block:: cue

    sample_string1: sample_string2
    sample_string1: sample_string3

    sample_string2: string
    sample_string2: =~"^abc_[a-z]{2}$"

    sample_string3: =~"^[a-z]{3}_de$"
    sample_string3: "abc_de"

は、

.. code-block:: cue

    sample_string1: "abc_de"
    sample_string2: =~"^abc_[a-z]{2}$"
    sample_string3: "abc_de"

に評価される。 ``=~"^abc_[a-z]{2}$"`` より ``"abc_de"`` の方が大きくこれが最大の要素であるため、 ``sample_string1`` はこの値になる。マージは結合的であるため、どこを括り出しても結果は変わらない。一方で順序関係による階層が作れないようなものは、データ検証段階で弾かれる。例えば、

.. code-block:: cue

    sample_ill: string
    sample_ill: int

は、 ``conflicting values string and int`` というエラーが報告され、評価されない。マージにおいて大事なのは、順序による階層に違反してないか、そして順序関係から導かれる最小上界が何であるかだけということになる。ところで今までは最小上界がマージする対象の中に入っている例を見てきたが、必ずしも最小上界は記述された値の中にあるとは限らない。例えば、

.. code-block:: cue

    sample_obj1: sample_obj2
    sample_obj1: sample_obj3

    sample_obj2: sample_string1: "str1"
    sample_obj2: sample_string2: "str2"

    sample_obj3: sample_string2: string
    sample_obj3: sample_string3: "str3"

は、

.. code-block:: cue

    sample_obj1: {
        sample_string1: "str1"
        sample_string2: "str2"
        sample_string3: "str3"
    }
    sample_obj2: {
        sample_string1: "str1"
        sample_string2: "str2"
    }
    sample_obj3: {
        sample_string2: string
        sample_string3: "str3"
    }

に評価される。CUE 言語はもちろん構造体をサポートしている。 ``a: b: string`` という記述は ``a: { b: string }`` と言う記述の略記であり、

.. code-block:: cue

    sample_obj1: sample_obj2
    sample_obj1: sample_obj3

    sample_obj2: sample_string1: "str1"
    sample_obj2: sample_string2: "str2"

    sample_obj3: sample_string2: string
    sample_obj3: sample_string3: "str3"

は、

.. code-block:: cue

    {
        sample_obj1: sample_obj2
        sample_obj1: sample_obj3

        sample_obj2: { sample_string1: "str1" }
        sample_obj2: { sample_string2: "str2" }

        sample_obj3: { sample_string2: string }
        sample_obj3: { sample_string3: "str3" }
    }

と記述するのと等しい。つまり、 ``sample_obj1`` は ``{ sample_string1: "str1" }``、 ``{ sample_string2: "str2" }``、 ``{ sample_string2: string }``、 ``{ sample_string3: "str3" }`` の最小上界を値に持つと言うことであり、その値が ``{ sample_string1: "str1", sample_string2: "str2", sample_string3: "str3" }`` であるということになる。構造体の順序については少々複雑なので後で詳しく触れるが、基本的にはこのようにフィールドそれぞれをマージするような挙動をする。

この機能はかなり強力であり、今回のように一部の値を先行して埋めその際データ検証が通るようにしたい、複数の部分的なスキーマを統合したいなどといったデータ記述のマージに関する複雑な需要がありかつデータ検証も行いたいような場合にかなり効力を発揮する。さらに CUE 言語には、フィールドの内包表記、文字列に対するパラメータの差し込み、条件分岐といった多少複雑なテンプレート機能をサポートしており、これもデータ記述の統合に力を貸してくれるだろう。詳細は、 `CUE 言語の式に関するドキュメント <https://cuelang.org/docs/tutorials/tour/expressions/>`_ を見てもらうのがいいだろう。

さて、データ記述が分けられるとくれば、ファイル分割によりファイルシステムに沿ったデータ記述の断片管理をしたいというのも需要の一つだ。CUE 言語では、ディレクトリレイアウトと連動可能なモジュールシステムを備えており、ファイル分割もサポートしている。ディレクトリレイアウトとモジュールシステムを連動させるには、まずそのレイアウトのルートとなるディレクトリにモジュールに関する宣言を行う必要がある。これは、 ``cue`` コマンドを利用して行うことができ、レイアウトのルートとなるディレクトリに移動し、以下のコマンドを実行することで宣言できる::

    cue mod init [モジュール名]

モジュール名は、そのモジュールに所属するパッケージのルートパスとなる識別子で、ドメインとオプションでパスを指定する。例えば、 ``mizunashi.work/pkg`` といった感じだ。手元で試すだけならドメインは所有している必要はなく実在している必要もない。ひとまず他のモジュールと被らないようなドメイン名であれば問題ないだろう。さて、モジュールの宣言を行うと、 ``cue.mod`` というディレクトリが作られ、そこにモジュールに関するファイル群が配置されることになる。それぞれ

* ``module.cue`` ファイルはモジュールに関する情報が宣言されるファイルで、モジュール名が記述されることになる。
* ``pkg`` ディレクトリは、外部モジュールが置かれるディレクトリで、ここに他のモジュールをダウンロードしてそのモジュール名に合わせて配置すると、そのモジュールも読み込めるようになる。
* ``usr`` ディレクトリは使ったことがないのでよく分からないが、使用する CUE 言語のユーザ定義拡張などを置く場所のようだ。

この ``cue.mod`` が置かれたディレクトリについて、 ``a/b/sample.cue`` といったファイルを以下の内容で作る:

.. code-block:: cue

    package sample

    sample_string: "abc"

この時、モジュール名が ``mizunashi.work/pkg`` であれば、

.. code-block:: cue

    import "mizunashi.work/pkg/a/b:sample"

    sample_obj: imported: sample

は

.. code-block:: cue

    sample_obj: {
        imported: {
            sample_string: "abc"
        }
    }

に評価される。基本的にパス名にコロン区切りでパッケージ名を繋げることで、そのパッケージのパスになる。なお、パッケージ自体を複数ファイルに分割することもでき、 ``a/sample/obj1.cue`` を

.. code-block:: cue

    package sample

    sample_string2: "abc2"

``a/sample/obj2.cue`` を

.. code-block:: cue

    package sample

    sample_string3: "abc3"

とすると、

.. code-block:: cue

    import "mizunashi.work/pkg/a/sample"

    sample_obj: imported: sample

は

.. code-block:: cue

    sample_obj: {
        imported: {
            sample_string2: "abc2"
            sample_string3: "abc3"
        }
    }

に評価される。この機能により、基本的に CUE 言語にファイル分割も任せれば、特に Ansible などの変数の利用側でファイル分割の機能を利用しなくてもよくなり、より柔軟で安全なデータ記述管理ができる。

Ansible での利用
-------------------------

では、この CUE を使って僕が Ansible の変数をどうやって管理しているかも紹介していこう。実例は、https://github.com/mizunashi-mana/mizunashi-work-playbook にある。

Ansible での CUE による変数管理の場合、僕は基本的に以下のような構成にするようにしている:

* インベントリディレクトリを CUE モジュールのルートとする。
* ロール一つ一つをパッケージとするそのロールのスキーマを ``roles/*/schema.cue`` に書く。
    - 例えば、 ``roles/node_exporter`` という Node Exporter のインストールロールがある時、 ``roles/node_exporter/schema.cue`` に ``node_exporter`` パッケージとして、そのロールで使用する変数を記述する。
    - 記述例は、https://github.com/mizunashi-mana/mizunashi-work-playbook/blob/389cfd78428dc8ede6fa941d7e2014fb47d3a136/roles/node_exporter/schema.cue とかを参照してもらうのが良いだろう。
* インベントリグループ毎の変数のスキーマを、 ``schemas/*.cue`` にグループの名前に合わせたパッケージを書く。
    - 必ずしもこれはグループを発行する必要はないし、グループに対応させる必要もない。が、グループに対応させておくと分かりやすい。
    - 記述例は、https://github.com/mizunashi-mana/mizunashi-work-playbook/blob/389cfd78428dc8ede6fa941d7e2014fb47d3a136/roles/node_exporter/schema.cue とかを参照してもらうのが良いだろう。
    - 基本的には、そのグループでプロビジョンが必要なロールのスキーマを読み込むだけで良い。上の記述例がどういう意味論を持つかは後ほど詳しく見る。
* その他共通化したいスキーマや設定などは適宜ディレクトリを作ってパッケージを作る。
* グループ変数、ホスト変数生成用の CUE ファイルを、生成先のディレクトリに生成先の名前に合わせて作る。
    - 上で作ったスキーマを適宜読み込み、グループに対しての変数を設定していく
    - 記述例は、https://github.com/mizunashi-mana/mizunashi-work-playbook/blob/389cfd78428dc8ede6fa941d7e2014fb47d3a136/group_vars/internal_vagrant.cue とかを参照してもらうのが良いだろう。
    - 上の記述例では、スキーマにない変数が間違って設定されないような工夫を施している。この記述例がどう言う意味論を持つかは後ほど詳しく見る。

そして、 ``cue export --out yaml group_vars/sample.cue --outfile group_vars/sample.yml`` といった具合にグループ変数、ホスト変数を生成する。基本生成するのは末端のグループだけで、後のグループの変数は CUE 側でマージしておけば、適切に共通化部分をコントロールでき、また最終的に適用される変数が分かりやすい。

ただ、このままだと対応できないものが一つだけあって、それが Ansible Vault だ。Ansible Vault は、暗号化済みファイルの注入か YAML カスタムタグでの注入しかサポートしてないっぽくて、CUE 言語では直接の対応はできない。そこで、僕は純粋に CUE 言語で対応するのはやめて、

.. code-block:: cue

    sample_vault: "__ansible_vault": """
    $ANSIBLE_VAULT;1.1;AES256
    ...
    """

みたいな入力が与えられた時、

.. code-block:: yaml

    sample_vault: !vault |
        $ANSIBLE_VAULT;1.1;AES256
        ...

に変換する `簡単な Python スクリプト <https://github.com/mizunashi-mana/mizunashi-work-playbook/blob/389cfd78428dc8ede6fa941d7e2014fb47d3a136/cue_compiler/compiler.py>`_ を書いている。そこまで大したことはしてなくて、 ``cue export`` で一旦上の CUE を YAML に変換した後、 ``"__ansible_vault": ...`` みたいなオブジェクトを探して ``!vault ...`` に変換し、YAML として再度出力してるというだけだ。ここら辺も CUE だけで完結できればかっこいいんだろうが、まあとりあえずこれで困ってない。他にいいやり方知ってたら教えて欲しい。

CUE の構造体とその開閉
--------------------------

さて、基本上記のような構成で、Ansible の変数をスキーマベースでファイル分割を怯えずに管理できる。ただ、幾つか CUE の構造体についてさらに知っておくと色々やりやすくなるので、最後にその点を触れておく。

まず、 CUE の構造体には開いているか閉じているかという属性がある。開いている構造体は、自身が含んでいないフィールドを持つ構造体でも、自身が含んでいるフィールドのみ互換性があれば順序がつく。つまり、 ``{ a: int }`` という開いた構造体があった場合、 ``{ a: 1, b: string }`` といった ``a`` フィールドは互換性があり、さらに ``b`` というフィールドが追加されているような構造体との間に順序がつく。このため、マージの際フィールドを新たに追加することが許可される。しかし、閉じた構造体はこういうことは許されない。つまり、 ``close({ a: int })`` と ``{ a: 1, b: string }`` の間には順序がつかず、同じ階層にも所属できない。ここで、 ``close({ a: int })`` は ``{ a: int }`` の閉じたバージョンだ。実際に

.. code-block:: cue

    sample_ill: close({ a: int })
    sample_ill: { a: 1, b: string }

は評価されず、 ``sample_ill.b: field not allowed`` というエラーが出る。閉じた構造体は、自身が持つフィールドと互換性のある範囲しかマージができない。例えば上記の例を逆に

.. code-block:: cue

    sample_obj: close({ a: 1, b: string })
    sample_obj: { a: int }

とすると、これは評価され、

.. code-block:: cue

    sample_obj: {
        a: 1
        b: string
    }

という結果になる。この閉じた構造体を使うことで、スキーマで定義されたフィールド以外の宣言がないことを検証できる。これは、更新忘れなどを防ぐのに役にたつ。

ここで、何点か注意して置く必要がある。一つ目は、宣言された構造体が開いたものになるか、閉じたものになるかの条件だ。基本的に ``{}`` で囲まれて宣言された構造体は開いた構造体になり、 ``close`` でさらに囲むと閉じた構造体になる。ただし、宣言の仕方によっては ``close`` を書かなくてもデフォルトで閉じた構造体になることがある。それが、秘匿フィールド (hidden field) での宣言の場合だ。秘匿フィールドは、 ``#`` から始まるフィールドで、このフィールドは最終結果からは除外される。例えば、

.. code-block:: cue

    #sample_hidden: 1
    sample_str: "str"

は、

.. code-block:: cue

    sample_str: "str"

に評価される。秘匿フィールドではなく、最終結果に含めるフィールドとして宣言するには、

.. code-block:: cue

    "#sample_int": 1
    sample_str: "str"

というように、クオートで囲む必要がある。秘匿フィールドは、スキーマの宣言や、最終結果に含めたくないが CUE 上での共通化のための宣言を行うために便利だ。さて、この秘匿フィールドは、基本宣言がそれだけで完結していることが多いため、デフォルトで再帰的に構造体が閉じるようになっている。実際に試してみると、例えば

.. code-block:: cue

    #sample_hidden: a: 1

    sample_obj: #sample_hidden
    sample_obj: b: 2

は、評価されず、 ``sample_obj.b: field not allowed`` というエラーが出る。注意したいのは、秘匿フィールドの宣言自体では、開いた構造体が使えるということだ。例えば、

.. code-block:: cue

    #sample_hidden: a: 1
    #sample_hidden: b: 2

    sample_obj: #sample_hidden

は、問題なく

.. code-block:: cue

    sample_obj: {
        a: 1
        b: 2
    }

と評価される。あくまで秘匿フィールドを使用する際には閉じた構造体として扱われるということだ。このことに注意しながら、秘匿フィールドを交えてデータ記述を行うと、より柔軟なデータ記述ができるだろう。

もう一つの注意点は、構造体のマージには実は幾つか構文があり、それぞれ異なる意味論を持っているということだ。今まで基本的に使ってきた

.. code-block:: cue

    sample_obj: { a: 1, b: 2 }
    sample_obj: { b: int, c: 3 }

という記述は、以下と等価になる:

.. code-block:: cue

    sample_obj: { a: 1, b: 2 } & { b: int, c: 3 }

これは、今までのマージが、マージ対象の最小上界であるということが分かりやすい。もちろん、

.. code-block:: cue

    sample_obj: close({ a: 1, b: int })
    sample_obj: { b: 2 }

も

.. code-block:: cue

    sample_obj: close({ a: 1, b: int }) & { b: 2 }

と記述できる。それに対して、構造体のマージは埋め込み (embedding) と呼ばれる他の意味論を持つ構文も用意されている:

.. code-block:: cue

    sample_obj: {
        close({ a: 1, b: int })
        b: 2
        { c: 3 }
    }

この記述を評価すると、

.. code-block:: cue

    sample_obj: {
        a: 1
        b: 2
        c: 3
    }

になる。これは、

.. code-block:: cue

    sample_obj: close({ a: 1, b: int }) & {
        b: 2
        c: 3
    }

が評価されずエラーになるのと対照的だ。埋め込みは、マージの対象に対して単に最小上界を取るのではなく、構造体のフィールドを全て展開してからマージを行う。このため、閉じた構造体も開いた構造体と同じようにマージされる。ただし、マージ対象の中に一つでも閉じた構造体がある場合、マージされた構造体も閉じた構造体になる。これらを同じ構文だと思うと色々ハマるので注意が必要だが、これらの構文をうまく使い分けできれば、柔軟なデータ管理ができるだろう。

まとめ
---------

というわけで、CUE 言語の紹介と、それを Ansible の変数管理に活用する構成例を紹介した。最初の頃は構造体やモジュールの仕様につまづき、色々苦労もしたが、今は結構色々助かってて安心して変数を増やしたり削除したりできるようになったし、出力される変数のファイル自体は1ファイルなので、変数の差分なども見やすくなって色々重宝してる。まだ色々手探りなところもあるものの、現状そうつまづくことはないかなあという感じで、後の懸念点は安定性がどんくらいなのかなあということぐらいか。ま、興味があれば使ってみて欲しい。

では、今回はこれで。
