Systemd ユニットのセキュリティスコアを改善する
=======================================================

:tags: systemd, セキュリティ, Linux
:category: 運用

systemd は Linux の標準的なシステム・サービスマネージャで、sysvinit の代替の init プロセスとなることを目指して開発されたソフトウェアである。長らく色々論争が行われ、紆余曲折はあったものの、現在ではほとんどの Linux ディストリビューションが標準的に init として採用している。SysV のサービスが秘伝の起動スクリプトにより管理されてきたのに対して、systemd ではサービスを記述するユニットファイルによりサービスを管理する。黎明期は systemd の方は様子見で、SysV 用の起動スクリプトだけを提供するプロジェクトも多かったが、現在は systemd ユニットを標準的に提供するプロジェクトも増えている。

さて、サービスを管理する上で重要なことの一つがセキュリティだ。サービスは基本的に長期間動くプロセスで、権限も大きく、他のサービスとの連携も行われる。そのため、外部からの攻撃にも晒されやすく、セキュリティホールにもなりやすい。もちろん、サービスコードそのものが万全なセキュリティを搭載しているのも重要だが、万全のセキュリティというものは存在しないため、サービスマネージャにおいてもあるサービスへの攻撃の防御と影響範囲の最小化を事前に行っておくのが良い。systemd はそのための機能をいくつか提供している。今回は、その機能を使用したユニットのセキュリティ改善方法を紹介する。

systemd によるセキュリティスコア
---------------------------------------

systemd にはセキュリティ機能の解析と、スコア評価を行う機能が提供されている:

::

    $ systemd-analyze security systemd-resolved
    NAME                                                        DESCRIPTION                                                                                         EXPOSURE
    ✓ SystemCallFilter=~@swap                                     System call allow list defined for service, and @swap is not included
    ✗ SystemCallFilter=~@resources                                System call allow list defined for service, and @resources is included (e.g. ioprio_set is allowed)      0.2
    ✓ SystemCallFilter=~@reboot                                   System call allow list defined for service, and @reboot is not included
    ✓ SystemCallFilter=~@raw-io                                   System call allow list defined for service, and @raw-io is not included
    ✗ SystemCallFilter=~@privileged                               System call allow list defined for service, and @privileged is included (e.g. chown is allowed)          0.2
    ...
    ✗ UMask=                                                      Files created by service are world-readable by default                                                   0.1

    → Overall exposure level for systemd-resolved.service: 2.1 OK 🙂

ロード済みのユニットに対して、 ``systemd-analyze security <ユニット名>`` の形で実行すると、セキュリティ設定の有効可否と設定レベルを出してくれる。レベルは 0 - 10 の間でつけられ、レベルが小さいほどセキュリティ機能の有効度が高く、レベルが大きいほど改善の余地があるということになる。

基本的にはこれを見ながら、バツマークがついてるところの中でスコアが高いものから対応していくのが良い。もちろん、何も考えずに設定を入れてもだめで、サービスコードとの兼ね合いを見ながら必要だと思われる設定だけをつけていけば良い。では、具体的にそれぞれ指摘される設定をそれぞれ見ていこう。

systemd のセキュリティ機能
-----------------------------

systemd のセキュリティ機能は、概ね3種類ある:

サンドボックス化
    サービスのプロセス専用の隔離リソースを作り、影響が他のサービスや関係ないリソースに及ばないようにする機能。

制限
    サービスに不要なリソースへのアクセスを制限することで、影響を最低限に抑える機能。

デフォルト挙動改善
    影響が大きい機能のデフォルトの挙動を、セキュリティ的に望ましい方向に改善する機能。

基本的に外部からの攻撃というものは、典型的なパターンがある:

1. 意図しないコード実行やリソース作成を行う
2. 情報を集め、攻撃者に送信し、さらに意図しないコード実行やリソース作成を行う
3. マルウェアやランサムウェアを埋め込み、外部へのさらなる攻撃への中継点にしたり、アクセスできるリソースの改変を行い不正な取引の材料にしたりする

逆に言えば、意図しないコード実行やリソース作成をしても情報を集められない、攻撃者に情報を送れないといった状況を作ったり、アクセスできるリソースを制限することで攻撃の影響範囲はかなり抑えられるということだ。つまり、サンドボックス化により他のリソースから隔離し、さらにできることを必要な範囲だけに制限することにより、典型的な攻撃は防御できる。もちろん、サービス自体やそこからアクセスが必要なリソースに脆弱性がありそれにより十分広い影響が出ることはあるため、サービスマネージャのセキュリティ機能だけに頼るのは問題だが、サービスのセキュリティに関して強力な補佐とはなるだろう。

では、それぞれ詳細を見ていく。まず、サンドボックス機能のスコアが高いものから:

``PrivateNetwork``
    専用のネットワーク名前空間を作り、それを使う。このオプションを有効にすると、ネットワークアクセスが内外問わず完全に遮断される。外部ネットワークへのアクセスなどを特にしないサービスでは、有効にしとくと良いだろう。これを有効化しておけば、ほとんどの攻撃から防御できるだろう。ただ、ネットワークアクセス自体は必要なサービスも多いことから、このオプションを有効にできるサービスはかなり限られるだろう。

    マニュアル: https://www.freedesktop.org/software/systemd/man/systemd.exec.html#PrivateNetwork=

``PrivateDevices``
    専用の ``/dev`` マウンティングを行い、基本的な擬似デバイスだけを見えるようにする。これにより、物理デバイスやシステムデバイスなどは見えなくなり、攻撃を受けても取得できる情報が限定される。物理デバイスやシステムデバイスにアクセスするようなサービスは限られる為、基本的には有効にしておくのが良いだろう。

    マニュアル: https://www.freedesktop.org/software/systemd/man/systemd.exec.html#PrivateDevices=

``PrivateMounts``
    マウント名前空間が専用に作成され、サービスが作成したマウントポイントがホストからは見えなくなる。これによりサービスが攻撃を受けて不正なマウントポイントを作成してしまったとしても、他のサービスには全く影響が出なくなるし、逆に他のサービスからサービスのマウントポイントが攻撃されることもなくなる。基本的に、そもそもマウントポイントを作成されるサービスは限られるため、マウント権限自体を無効化しとくのがいいだろうが、その上でだったり、他のサービスとマウントポイントを共有することがない場合は、このオプションを有効化しておくのがいいだろう。

    マニュアル: https://www.freedesktop.org/software/systemd/man/systemd.exec.html#PrivateMounts=

``PrivateTmp``
    専用の一時ディレクトリ用ファイル名前空間を作り、それを使う。これにより、他のプロセスの一時ファイルを一時ディレクトリ経由で弄ることができなくなる。一時ディレクトリを利用したサービスロジックは、TOCTOU に関する問題を利用したセキュリティホールを持ちやすい為、攻撃対象になりやすい。また、他プロセスと一時ディレクトリを共有する必要は通常ないため、基本的には有効化しておくのが良いだろう。

    マニュアル: https://www.freedesktop.org/software/systemd/man/systemd.exec.html#PrivateTmp=

``PrivateUsers``
    サービス専用のユーザ名前空間を作り、root ユーザと実行ユーザだけがそのままマッピングし、他のユーザは nobody ユーザとしてマッピングした上で使う。これにより、リソースに対して紐づくユーザ情報が取得できないようになる。他ユーザと連携することがなければ、基本的に有効化しておくのがいいだろう。

    マニュアル: https://www.freedesktop.org/software/systemd/man/systemd.exec.html#PrivateUsers=

``KeyringMode``
    セッションキーリングの設定制御を行う。 ``KeyringMode=inherit`` を指定する場合は、特別なキーリング設定は行わず、カーネルのデフォルト動作が適用される。 ``KeyringMode=private`` を指定する場合は、専用のセッションキーリングが作成され、どのユーザキーリングにもリンクされない。 ``KeyringMode=shared`` を指定する場合は、 ``private`` と同様専用のセッションキーリングが発行されるが、実行ユーザのキーリングにはリンクされる。デフォルトでは、 ``private`` が指定されており、基本的にはここから変更する必要はないだろう。キーリングで管理される認証情報を他のサービスと共有することは、他のサービスが攻撃を受けた時重大な被害が及びかねない。基本的には他のサービスと切り離しておくべきだろう。

    マニュアル: https://www.freedesktop.org/software/systemd/man/systemd.exec.html#KeyringMode=

``RootDirectory`` / ``RootImage``
    RootDirectory は chroot を使ってサービスのルートディレクトリを指定したディレクトリに変えることで、そのディレクトリより上位のディレクトリへのアクセスをファイルシステム上できなくする。RootImage はさらに、ディレクトリの代わりにデバイスノードを指定することでそのデバイスノードの隔離されたファイルシステムを使うようにする。

    ファイルは重要なリソースで、それ故に攻撃対象になりやすい。ディレクトリトラバーサルなどの脆弱性をサービスコードが持っていると、他サービスやシステムリソースなどへのアクセスもできてしまい、それにより重大な結果をもたらす恐れもある。これを避ける根本的な手段は、そもそも他サービスやシステムリソースにファイルシステム上辿り着けないようにするのが一番良い。その時に出番となるオプション。ただ、流石にルートディレクトリを変えてしまうと、動的ライブラリの解決なども出来なくなるので、セキュリティ的に強力な反面運用は難しい。まあ、可能なら設定しておくぐらいでいいだろう。

    マニュアル:

    * https://www.freedesktop.org/software/systemd/man/systemd.exec.html#RootDirectory=
    * https://www.freedesktop.org/software/systemd/man/systemd.exec.html#RootImage=

次に制限のための機能を見ていく:

``User`` / ``DynamicUser``
    このオプションは使ってる人も多いだろう。User オプションはサービスの実行ユーザを指定するオプション。DynamicUser も実行ユーザをルートユーザと異なるユーザにできるオプションで、こちらは有効にすると実行のたびに専用のユーザとグループを動的に作成して割り当ててくれる。

    さて、systemd のサービスはデフォルトではルートユーザで動く。ルートユーザは様々な特権的権限を持っており、全てのディレクトリへアクセスできるし改変もできる。権限を他のユーザに付与したりすることも可能だ。逆に言えばこのユーザが乗っ取られると大変なことになる。またルートユーザの証跡は追いにくく、いつ何をそのサービスがしたのかも分からなくなりがちだ。そこで、サービスの実行専用ユーザを作成し、実行に必要な最小限の権限だけを与えて実行ユーザとすることで、そのサービスが攻撃者に乗っ取られても影響範囲を抑えることができ、また何をされたかの証跡が追いやすくなる。基本的にどちらかのオプションを使うことで、ルートユーザでのサービス実行は避けるのが良いだろう。

    マニュアル:

    * https://www.freedesktop.org/software/systemd/man/systemd.exec.html#User=
    * https://www.freedesktop.org/software/systemd/man/systemd.exec.html#DynamicUser=

``RestrictNamespaces``
    アクセス可能な名前空間機能の種類を指定する。せっかくサンドボックス機能で名前空間を分離しても、他の名前空間が触れては意味がない。そこでこのオプションを併用することで、隔離をより強固にできる。名前空間管理機能に触るサービスは限られるだろうから、基本的には ``RestrictNamespaces=true`` で全ての種類について有効化しておくのが良いだろう。もし、名前空間管理機能にアクセスする場合も、必要な種類のみホワイトリスト形式で除外しておくのが良いだろう。

    マニュアル:

    * https://www.freedesktop.org/software/systemd/man/systemd.exec.html#RestrictNamespaces=
    * https://man7.org/linux/man-pages/man7/namespaces.7.html

``RestrictAddressFamilies=~AF_(INET|INET6)``
    IPv4 / IPv6 ソケットの作成を制限する。基本的に攻撃に持続性を持たせるには、リモートアクセスが必須となる。そうなると、攻撃元でリモートアクセス用のエンドポイントを作るか、攻撃先にリモートアクセス用のエンドポイントを作るかしかない。その一方が封じられることは、セキュリティの向上につながるだろう。もしサービスが IP ソケット作成を必要としないなら、指定しておくのが良いだろう。

    マニュアル:

    * https://www.freedesktop.org/software/systemd/man/systemd.exec.html#RestrictAddressFamilies=
    * https://man7.org/linux/man-pages/man2/socket.2.html

``ProtectControlGroups``
    cgroup を読み込み専用にし、書き込みを無効化する。cgroup を変更できると、他サービスからリソースを奪ったり、保護機能を無効化したりできる。基本的なコンテナ管理サービス以外で cgroup の書き込み権限は必要ないだろうから、指定しておくのがいいだろう。

    マニュアル: https://www.freedesktop.org/software/systemd/man/systemd.exec.html#ProtectControlGroups=

``ProtectKernelModules``
    カーネルモジュールの読み込みを無効化する。カーネルモジュールによるカーネル機能の拡張は、サービス提供者だけでなく、攻撃者にとっても強力な助っ人となり得る。カーネルモジュールの読み込みが必要なサービスは限られるだろうから、基本的には有効化しておくのがいいだろう。

    マニュアル: https://www.freedesktop.org/software/systemd/man/systemd.exec.html#ProtectKernelModules=

``ProtectKernelTunables``
    カーネル変数を読み込み専用にし、書き込みを無効化する。カーネル変数の変更は、カーネルの重要な保護機能を無効化したり、攻撃に利用できるような機能の有効化に繋げられかねない。また、基本的に実行時に変更を必要とすることはないため、有効化しておくのが良いだろう。

    マニュアル: https://www.freedesktop.org/software/systemd/man/systemd.exec.html#ProtectKernelTunables=

``ProtectKernelLogs``
    カーネルログのリングバッファを読み書きする権限を無効化する。カーネルログを取得すると、ホストの情報を取得し、さらなる攻撃に繋げられかねない。基本的にカーネルログを読み書きするサービスは限られるだろうから、有効化しておくのが良いだろう。

    マニュアル: https://www.freedesktop.org/software/systemd/man/systemd.exec.html#ProtectKernelLogs=

``ProtectClock``
    サービスが、ハードウェアクロック、システムクロックの変更をできなくするオプション。時間に関する判定をバグらせるとセキュリティホールにつながることも多い。特にタイムスタンプを任意に固定できれば、攻撃の幅が広がることもある。まあ基本的にクロックを変更するサービスはあまりないと思うし、攻撃手段として利用できなくしておくのに越したことはないだろうから、基本的には有効にしておくのが良いだろう。

    マニュアル: https://www.freedesktop.org/software/systemd/man/systemd.exec.html#ProtectClock=

``ProtectHome``
    サービスから、 ``/home``、 ``/root``、 ``/run/user`` を見えなくする、または読み込み専用にするオプション。 ``ProtectHome=true`` と指定した場合、これらのディレクトリをサービス用の名前空間から隔離し、サービスから見えないようにする。 ``ProtectHome=read-only`` と指定した場合、これらのディレクトリを読み込み専用でマウントする。 ``ProtectHome=tmpfs`` と指定した場合、一時ディレクトリとしてこれらのディレクトリを読み込み専用で作り、サービスからは実際のディレクトリではなく一時ディレクトリの方を参照するようにする。基本的にはサービスが実行時に書き込みを行うのは、実行時ディレクトリ、状態ディレクトリ、またはキャッシュディレクトリであるべきであり、ホームディレクトリを参照するべきではない。基本的に多くのサービスは、実行時のデータディレクトリを指定できる場合が多い。それらの機能を通してホームディレクトリ以外を実行時ディレクトリとして選べるなら、書き込み可能な範囲を押さえ、他のユーザへの影響をなくすためにも有効化しておくのが良いだろう。なお、たまにホームディレクトリが全く存在しないと立ち上がらないサービスもあるため、その場合は一時ディレクトリによるマウントを指定しておくのが良いだろう。

    マニュアル: https://www.freedesktop.org/software/systemd/man/systemd.exec.html#ProtectHome=

``ProtectSystem``
    システムディレクトリを読み込み専用でマウントし、書き込みを禁止するオプション。 ``ProtectSystem=true`` の場合 ``/usr`` とブートローダのディレクトリが、 ``ProtectSystem=full`` の場合さらに ``/etc`` ディレクトリが読み込み専用でマウントされる。 ``ProtectSystem=strict`` の場合は、API 用の ``/dev``、 ``/proc``、 ``/sys`` 以外のディレクトリが全て読み込み専用でマウントされる。これは後述する ``ReadWritePaths`` で例外を設定できる。また、読み込み専用でマウントされる対象はあくまでホストのディレクトリであり、 ``PrivateTmp`` などのサンドボックス機能で生成されるサービス専用のマウントポイントは書き込み可能のまま維持される。ウイルスをインストールするにしろ、他のサービスを攻撃するにしろ、システムディレクトリに書き込みを行うことは攻撃の基本的な手段だ。よって、書き込み可能なディレクトリを少なくしておくことは、重要なセキュリティ施策になる。また、書き込みが起こるディレクトリを把握しておくことは、各サービスが攻撃を受けた時の影響範囲を把握する上で重要だ。基本的には有効にしておき、例外を ``ReadWritePaths`` にホワイトリスト形式で追加するのが良いだろう。

    マニュアル: https://www.freedesktop.org/software/systemd/man/systemd.exec.html#ProtectSystem=

``NoNewPrivileges``
    プロセスが execve を通して新たに特権を取得することを無効化する。これは、プロセスが特権昇格をできないようにする基本的な方法だ。これにより、攻撃による被害拡大を抑えることができる場合が多い。基本的には指定しておくのが良いだろう。

    マニュアル: https://www.freedesktop.org/software/systemd/man/systemd.exec.html#NoNewPrivileges=

``ProtectProc``
    プロセスのメタ情報の可視、不可視を制御する。 ``ProtectProc=noaccess`` を指定する場合、他のプロセスの情報を ``/proc`` ディレクトリから取得できなくなる。 ``ProtectProc=invisible`` を指定する場合、実行ユーザではない他のユーザにより実行されているプロセスの情報は隠され、取得できないようになる。 ``ProtectProc=ptraceable`` を指定する場合、 ``ptrace`` による他プロセス情報の取得を制限する。 ``ProtectProc=default`` を指定する場合、プロセスメタ情報へのアクセス、可視性は特に制限されない。プロセス情報取得は、攻撃者にどのようなサービスが動いているか知る機会を与え、さらなる被害拡大を招きかねない。基本的に他のサービスのプロセス情報を取得する必要があるサービスは限られるだろうから、 ``ProtectProc=invisible`` または ``ProtectProc=noaccess`` を指定しておくのが良いだろう。

    マニュアル: https://www.freedesktop.org/software/systemd/man/systemd.exec.html#ProtectProc=

``RestrictSUIDSGID``
    SUID、SGID 権限をファイルまたはディレクトリに設定するのを拒否する。SUID、SGID は、サービス自体に課せられた特権の制約を無視して、攻撃者が特権を行使するかなり大きなセキュリティホールとなりうる。実行時に SUID、SGID 権限を指定するサービスはほぼないだろうし、あったとしたらそれは代替案を探すべきだろう。なので、基本的には有効化しておくのが良いだろう。

    マニュアル: https://www.freedesktop.org/software/systemd/man/systemd.exec.html#RestrictSUIDSGID=

``RestrictAddressFamilies=~AF_PACKET``
    デバイスドライバレベルでの生パケット送受信用ソケット作成を制限する。生パケットレベルでの送受信は攻撃者が既存のファイアウォールルールなどを回避しながら、データ送受信を行う手段となりうる。生パケット送受信用ソケット作成を行うサービスはかなり限られるだろうから、指定しておくのが良い。

    マニュアル:

    * https://www.freedesktop.org/software/systemd/man/systemd.exec.html#RestrictAddressFamilies=
    * https://man7.org/linux/man-pages/man2/socket.2.html

``SystemCallArchitectures``
    後述の ``SystemCallFilter`` でフィルタするシステムコールの対象アーキテクチャを指定する。ネイティブのアーキテクチャを対象とする場合は、 ``SystemCallArchitectures=native`` と指定する。このオプションを指定しないと、 ``SystemCallFilter`` が効かない為、基本的に指定しておくべきだろう。また、ネイティブアーキテクチャ以外を指定する必要が生じる場面は普通なく、下手に範囲を広げるとフィルタリングを回避する穴を作りかねない。 ``SystemCallArchitectures=native`` の指定を基本的に入れておくのがいいだろう。

    マニュアル: https://www.freedesktop.org/software/systemd/man/systemd.exec.html#SystemCallArchitectures=

``SystemCallFilter=~@debug``
    システムコールの内、デバッグ、パフォーマンス計測機能に関するものの使用を禁じるオプション。もしこれらのシステムコールが呼び出された場合、サービスは SIGSYS シグナルと共に終了することになる。対象のシステムコール一覧は、 ``systemd-analyze syscall-filter @debug`` で見ることができる。 ``ptrace``、 ``perf_event_open`` などが対象。これらのシステムコールは、サービスの情報を詳細に取得し、攻撃者が被害を拡大させるための分析手段として使用できる。基本的に入れておくのが良いだろう。もし、デバッグ機能が必要だとしても、必要なシステムコールだけホワイトリストで追加し直すのが良い。具体的には、

    .. code-block:: ini

        SystemCallFilter=~@debug
        SystemCallFilter=ptrace

    と書くことで、デバッグ機能のうち、 ``ptrace`` システムコールだけを有効化することができる。必要なくなったら無効化しておくのが良いだろう。

    マニュアル:

    * https://www.freedesktop.org/software/systemd/man/systemd.exec.html#SystemCallFilter=
    * https://man7.org/linux/man-pages/man2/syscalls.2.html

``SystemCallFilter=~@swap``
    システムコールの内、スワップデバイスの有効化、無効化に関するものの使用を禁じるオプション。対象のシステムコール一覧は、 ``systemd-analyze syscall-filter @swap`` で見ることができる。主に ``swapon``、 ``swapoff`` の2つが対象。スワップデバイスの無効化は、メモリ不足を生じさせ、全体的なシステムの不具合を生じさせる手段になる。ファイアウォールなどがメモリ不足で落ちれば、ホストの保護は大幅に低下するだろう。スワップデバイス制御を行うサービスはかなり限られるだろうから、基本的に入れておくのが良いだろう。

    マニュアル:

    * https://www.freedesktop.org/software/systemd/man/systemd.exec.html#SystemCallFilter=
    * https://man7.org/linux/man-pages/man2/syscalls.2.html

``SystemCallFilter=~@clock``
    システムコールのうち、システムクロックの制御に関するものの使用を禁じるオプション。対象のシステムコール一覧は、 ``systemd-analyze syscall-filter @clock`` で見ることができる。主に ``adjtimex``、 ``settimeofday`` などが対象。時間に関する判定をバグらせるとセキュリティホールにつながることも多い。クロック制御を行うサービスはかなり限られるだろうから、基本的に入れておくのが良いだろう。

    マニュアル:

    * https://www.freedesktop.org/software/systemd/man/systemd.exec.html#SystemCallFilter=
    * https://man7.org/linux/man-pages/man2/syscalls.2.html

``SystemCallFilter=~@mount``
    システムコールのうち、ファイルシステムのマウント制御に関するものの使用を禁じるオプション。対象のシステムコール一覧は、 ``systemd-analyze syscall-filter @mount`` で見ることができる。主に ``mount``、 ``chroot`` などが対象。ファイルシステムのマウントは、ファイルスプーフィングの手段に使える。マウント制御を行うサービスはかなり限られるだろうから、基本的に入れておくのが良いだろう。

    マニュアル:

    * https://www.freedesktop.org/software/systemd/man/systemd.exec.html#SystemCallFilter=
    * https://man7.org/linux/man-pages/man2/syscalls.2.html

``SystemCallFilter=~@module``
    システムコールのうち、カーネルモジュールのロード制御に関するものの使用を禁じるオプション。対象のシステムコール一覧は、 ``systemd-analyze syscall-filter @module`` で見ることができる。主に ``init_module``、 ``delete_module`` などが対象。カーネルモジュールのロードを開放することは、攻撃者に被害拡大の手段を与えることも多い。カーネルモジュールのロードが必要なサービスはかなり限られるだろうから、基本的に入れておくのが良いだろう。

    マニュアル:

    * https://www.freedesktop.org/software/systemd/man/systemd.exec.html#SystemCallFilter=
    * https://man7.org/linux/man-pages/man2/syscalls.2.html

``SystemCallFilter=~@raw-io``
    システムコールのうち、低レベルの I/O ポートにアクセスするものの使用を禁じるオプション。対象のシステムコール一覧は、 ``systemd-analyze syscall-filter @raw-io`` で見ることができる。主に ``ioperm``、 ``pciconfig_read`` などが対象。通常は他のシステムコールで代用可能で、カーネルがよしなにやってくれるものを使う方が良い。わざわざ使わないものの使用を解禁し、攻撃者に情報を取得する手段を提供する必要はないため、基本的に入れておくのが良いだろう。

    マニュアル:

    * https://www.freedesktop.org/software/systemd/man/systemd.exec.html#SystemCallFilter=
    * https://man7.org/linux/man-pages/man2/syscalls.2.html

``SystemCallFilter=~@reboot``
    システムコールのうち、再起動に関するものの使用を禁じるオプション。対象のシステムコール一覧は、 ``systemd-analyze syscall-filter @reboot`` で見ることができる。主に ``reboot``、 ``kexec_load`` などが対象。再起動を攻撃者に許すと、カーネルモジュールの読み込みをより柔軟にできるようにしたり、再起動直後のセキュリティ保護が万全でない状態を攻撃対象にされかねない。再起動を行うサービスはかなり限られるだろうから、基本的に入れておくのが良いだろう。

    マニュアル:

    * https://www.freedesktop.org/software/systemd/man/systemd.exec.html#SystemCallFilter=
    * https://man7.org/linux/man-pages/man2/syscalls.2.html

``IPAddressAllow`` / ``IPAddressDeny``
    IP パケットの送受信において、許可するアドレス、拒否するアドレスを指定する。IPv4、IPv6 アドレスを、直接またはレンジで指定できる。 ``IPAddressAllow`` が優先となる。特別な識別子がいくつか用意されており、

    * ``any`` は ``0.0.0.0/0 ::/0`` のエイリアス
    * ``localhost`` は ``127.0.0.0/8 ::1/128`` のエイリアス
    * ``link-local`` は ``169.254.0.0/16 fe80::/64`` のエイリアス
    * ``multicast`` は ``224.0.0.0/4 ff00::/8`` のエイリアス

    としてそれぞれ機能する。ホワイトリストとして使う際は、 ``IPAddressAllow`` にリストを指定し、 ``IPAddressDeny=any`` も合わせて指定する。ブラックリストの場合は、 ``IPAddressDeny`` だけ指定する。使うネットワークが限られているサービスについては、わざわざサービス側に IP フィルタの仕組みを用意しなくても、この機能を使うことでフィルタリングが可能だ。また、攻撃に持続性を持たせるためには、基本的にネットワーク通信は必須であり、その送受信を封じることは強力なセキュリティ保護となる。ネットワーク通信を行わない場合は、 ``IPAddressDeny=any`` を指定しておき、リモートアクセスを完全に無効化しておくのが良いだろう。

    マニュアル: https://www.freedesktop.org/software/systemd/man/systemd.resource-control.html#IPAddressAllow=ADDRESS%5B/PREFIXLENGTH%5D%E2%80%A6

``DeviceAllow``
    デバイスノードに対するアクセス制御を指定する。アクセスを許可するデバイスとその読み込み (``r``)、書き込み (``w``)、作成 (``m``) を許可するかを指定する。物理デバイスに対するアクセスを ``PrivateDevices`` で全て無効化している場合は、特に気にしなくていいだろう。デバイスへの直接のアクセスを提供することは、様々なセキュリティ保護を回避しつつ、影響の大きな攻撃を成立させる手段となりうる。もし、物理デバイスに一部でもアクセスが必要な場合は、必要なアクセスのみを指定するのが良いだろう。

    マニュアル: https://www.freedesktop.org/software/systemd/man/systemd.resource-control.html#DeviceAllow=

``NotifyAccess``
    systemd のサービスステータス通知へのアクセスを制御する。このオプションはデフォルトで無効化されているため、通常気にすることはないだろう。必要な場合は、必要な範囲のステータス通知だけを受け取るようにしておくのが良いだろう。

    マニュアル:

    * https://www.freedesktop.org/software/systemd/man/systemd.service.html#NotifyAccess=
    * https://www.freedesktop.org/software/systemd/man/sd_notify.html

``SystemCallFilter=~@privileged``
    システムコールのうち、特権が必要なものに関するものの使用を禁じるオプション。対象のシステムコール一覧は、 ``systemd-analyze syscall-filter @privileged`` で見ることができる。対象には、 ``@swap``、 ``@clock`` などを始め、キャパビリティを必要とするものは全部含まれる。流石にキャパビリティを1つ2つ必要とするサービスは普通にあるため、純粋にこのオプションを有効化できない場面もあるだろう。ただ、特権を必要とするシステムコールは影響力が強いからこそ特権が必要となっているため、セキュリティ的にサービスが必要とする特権は把握しておいた方が良いだろう。その意味でも、基本的にはこのオプションを指定しておいて必要となるシステムコールはホワイトリスト形式で指定しておくのが良いだろう。

    マニュアル:

    * https://www.freedesktop.org/software/systemd/man/systemd.exec.html#SystemCallFilter=
    * https://man7.org/linux/man-pages/man2/syscalls.2.html

``SystemCallFilter=~@resources``
    システムコールのうち、リソースや実行計画の変更に関するものの使用を禁じるオプション。対象のシステムコール一覧は、 ``systemd-analyze syscall-filter @resources`` で見ることができる。主に ``setrlimit``、 ``setpriority`` などが対象。これらのシステムコールによるリソースの占有設定などはシステム全体に悪影響を与える手段となりうる。リソースや実行計画の変更は、重要なサービスのパフォーマンスを確保するのに必要な場合もあるため、完全無効化は難しい場面もあるかもしれないが、基本的に設定しておいて許可するものだけをホワイトリスト形式で指定しておくのが良いだろう。

    マニュアル:

    * https://www.freedesktop.org/software/systemd/man/systemd.exec.html#SystemCallFilter=
    * https://man7.org/linux/man-pages/man2/syscalls.2.html

``RestrictRealtime``
    リアルタイムスケジューリングの有効化を無効にする。スケジューリングの設定は、CPUリソースを占有することによる他サービスの不調を招く手段となりうる。スケジューリング設定は、重要なサービスのパフォーマンスを確保するのに必要な場合もあるが、もし必要ないなら基本指定しておき、必要なものだけにスケジューリング設定を解放しておくのが良いだろう。

    マニュアル: https://www.freedesktop.org/software/systemd/man/systemd.exec.html#RestrictRealtime=

``SystemCallFilter=~@obsolete``
    システムコールのうち、使用されないもの、非推奨のもの、未実装のものの使用を禁じるオプション。対象のシステムコール一覧は、 ``systemd-analyze syscall-filter @obsolete`` で見ることができる。主に ``create_module``、 ``gtty`` などが対象。これらのシステムコールを必要とする状況はほぼないだろうから、このオプションは基本指定しておくのが良いだろう。

    マニュアル:

    * https://www.freedesktop.org/software/systemd/man/systemd.exec.html#SystemCallFilter=
    * https://man7.org/linux/man-pages/man2/syscalls.2.html

``SystemCallFilter=~@cpu-emulation``
    システムコールのうち、使用されないもの、非推奨のもの、未実装のものの使用を禁じるオプション。対象のシステムコール一覧は、 ``systemd-analyze syscall-filter @obsolete`` で見ることができる。主に ``create_module``、 ``gtty`` などが対象。これらのシステムコールを必要とする状況はほぼないだろうから、このオプションは基本指定しておくのが良いだろう。

    マニュアル:

    * https://www.freedesktop.org/software/systemd/man/systemd.exec.html#SystemCallFilter=
    * https://man7.org/linux/man-pages/man2/syscalls.2.html

``RestrictAddressFamilies=~AF_NETLINK``
    Service may allocate netlink sockets (ref: https://www.freedesktop.org/software/systemd/man/systemd.exec.html#RestrictAddressFamilies=)

``SupplementaryGroups``
    Service runs with supplementary groups (ref: https://www.freedesktop.org/software/systemd/man/systemd.exec.html#SupplementaryGroups=)

``LockPersonality``
    パーソナリティ、つまりカーネル実行ドメインの変更を無効化する。パーソナリティの指定はあまりテストされておらず脆弱性の元になりかねない。またほとんどのサービスで必要となることはないだろうから、基本的に指定しておくのが良いだろう。

    マニュアル: https://www.freedesktop.org/software/systemd/man/systemd.exec.html#LockPersonality=

``MemoryDenyWriteExecute``
    書き込み可能かつ実行可能なメモリマッピングを作成したり、既存のメモリマッピングを実行可能になるよう変更することを禁じるオプション。外部からの影響が大きいリモートコード実行の脆弱性は、動的に実行可能コードを埋め込んだり、変更したりできることに起因することが多いため、このオプションはそのような脆弱性をついた攻撃からの保護において有用だ。ただ、JIT 方式を採用しているようなランタイムなどは、その仕組み上そもそも動的に実行可能コードを変更することにより動くため、そのようなランタイムを採用しているサービスではこのオプションは有効化できない。そういう場合以外は基本的に有効化しておくのが良いだろう。

    マニュアル: https://www.freedesktop.org/software/systemd/man/systemd.exec.html#MemoryDenyWriteExecute=

``Delegate``
    リソース制御の分割委任を有効化する。適切なリソースの設計と委任設定がされていれば問題ないが、その適切な管理は複雑で難しく、またほとんどのサービスでは必要ないことから、特に有効化する必要はないだろう。また、デフォルトで無効化されているため、基本的に気にする必要はないだろう。

    マニュアル:

    * https://www.freedesktop.org/software/systemd/man/systemd.resource-control.html#Delegate=
    * https://systemd.io/CGROUP_DELEGATION/

``ProtectHostname``
    ホスト名、ドメイン名の名前空間を専用に作成し、さらに変更できないようにする。ホスト名の解決は、基本的にどのドメイン解決より優先される為、ホスト名変更はスプーフィング攻撃などを行う手段として機能しやすい。ホスト名の変更が必要なサービスはかなり限られるだろうから、基本的には有効化しておくのがいいだろう。

    マニュアル: https://www.freedesktop.org/software/systemd/man/systemd.exec.html#ProtectHostname=

``RestrictAddressFamilies=~AF_UNIX``
    UNIX ソケットの作成を制限する。UNIX ソケットは外部からのリモートアクセスには直接利用できないが、他のサービスからのデータ通信を、サンドボックス機能などを回避して行う手段となりうる。もしサービスが UNIX ソケット作成を必要としないなら、指定しておくのが良いだろう。

    マニュアル:

    * https://www.freedesktop.org/software/systemd/man/systemd.exec.html#RestrictAddressFamilies=
    * https://man7.org/linux/man-pages/man2/socket.2.html

``ProcSubset``
    プロセス管理と情報取得用の ``/proc`` ファイルシステムへのアクセスを制限する。 ``ProcSubset=pid`` を指定した場合、 ``/proc`` にプロセス情報は置かれず PID 毎のディレクトリだけが生成される。 ``ProcSubset=all`` を指定した場合、通常の挙動になる。 ``ProtectProc`` が指定されていれば、デフォルトの挙動でもそれほど大きな問題にはならないと思われるが、 ``ProcSubset=pid`` でサービスが問題なく動くなら、指定しておく分にはその方が良いだろう。

    マニュアル: https://www.freedesktop.org/software/systemd/man/systemd.exec.html#ProcSubset=

最後に挙動改善機能:

``CapabilityBoundingSet``
    キャパビリティ境界を設定するオプション。execve 実行時に引き継げるキャパビリティを指定する。任意コード実行のセキュリティホールがあった時に、本体プロセスのキャパビリティが引き継げてしまうと、被害が広がってしまう可能性がある。キャパビリティ境界はデフォルトでは全てのキャパビリティが引き継がれるが、基本的には引き継がなくていいケースが多い。その場合、 ``CapabilityBoundingSet=`` と指定することで全てのキャパビリティの引き継ぎを無効化できる。また、引き継ぐ必要がある場合も、引き継ぐ必要があるものをホワイトリストで指定しておいた方が良い。その場合は必要なキャパビリティを明示的に指定しておくのが良いだろう。

    マニュアル:

    * https://www.freedesktop.org/software/systemd/man/systemd.exec.html#CapabilityBoundingSet=
    * https://man7.org/linux/man-pages/man7/capabilities.7.html

``AmbientCapabilities``
    キャパビリティ環境を設定するオプション。キャパビリティ環境は、特権ユーザにより実行されたプロセスでなくても、キャパビリティを与える手段の一つ。特権を行使できるようにすることは攻撃の被害を深刻なものにする可能性があるため、使用を避けるようにするのがよい。デフォルトで指定なしなため、基本的には特に何もしなくても良いと思うが、もしキャパビリティ環境が必要な場合は必要なキャパビリティだけを指定するのが良いだろう。

    マニュアル:

    * https://www.freedesktop.org/software/systemd/man/systemd.exec.html#AmbientCapabilities=
    * https://man7.org/linux/man-pages/man7/capabilities.7.html

``RemoveIPC``
    サービス停止時ルートユーザ以外の実行ユーザ、グループが持つ IPC オブジェクトを破棄する。IPC オブジェクトを持続的に保持することは、攻撃者が持続的な攻撃を行う手段となりうる。また、サービス終了後も保持し続ける必要はない場合が多い。そのため、基本的には有効化しておくのが良いだろう。

    マニュアル: https://www.freedesktop.org/software/systemd/man/systemd.exec.html#RemoveIPC=

``UMask``
    ファイル作成時の権限マスク。指定されたマスクの部分の権限は取り除かれた状態でファイル、ディレクトリが作成される。デフォルトは ``0022`` であり、この場合グループ、その他のユーザの書き込み権限は取り除かれる。この場合、他のユーザからも作成されたファイルの読み取りは可能なため、他のサービスが攻撃された際、攻撃の余波を受ける可能性がある。作成されたファイルを他のユーザと共有する必要がないサービスは多いため、 ``0077`` を指定し実行ユーザのみに権限を与えるのが望ましい。また、もしファイルの共有が必要な場合は、共有用のグループで共有の範囲を絞っておくのが良いだろう。その場合は ``0027`` でグループのみ参照権限を与えるのが良いだろう。

    マニュアル:

    * https://www.freedesktop.org/software/systemd/man/systemd.exec.html#UMask=
    * https://man7.org/linux/man-pages/man2/umask.2.html

その他の有用な機能
--------------------------

さて、 ``systemd-analyze security`` では提示されないが、他にも幾つかセキュリティ的に有用な機能がある。それらも紹介しておこう:

``PrivateIPC``
    専用の IPC 名前空間を作成し、他のサービスの IPC 空間から隔離するサンドボックス化機能。他のサービスの IPC オブジェクトへの攻撃を避けられる他、名前競合によるクラッシュを避けるという効果も得られる。他のサービスと IPC オブジェクトを共有しないといけないサービスは稀なため、基本的には有効化しておくのが良いだろう。

``ReadWritePaths``
    ``ProtectSystem`` を有効化する場合、一時ディレクトリなどの一部のディレクトリを除いて、ファイルシステムが読み込み専用となる。しかし、実行時データを作成し、書き込むのはサービスの基本的な動作であり、一部ディレクトリに書き込みを許可したい場合はあるだろう。その場合、 ``ReadWritePaths`` にホワイトリスト形式でパスを指定することで、そのパスについては書き込みも許可される。また、このパス指定により、サービスが書き込みが起こるパスも明示され分かりやすくなるだろう。基本的には、 ``ProtectSystem`` と併用し、書き込みが起こるパスはホワイトリスト管理しておくのが良いだろう。

    マニュアル: https://www.freedesktop.org/software/systemd/man/systemd.exec.html#ReadWritePaths=

今後の systemd バージョンアップで、他にも機能が随時追加、改善されていくだろう。また、サービス自体も機能が改善されていく。それに合わせて定期的に ``systemd-analyze security`` によりセキュリティ機能の見直しを図っていくのが、セキュリティにおいて重要となるだろう。

まとめ
---------

というわけで、systemd におけるセキュリティ機能の紹介を行った。systemd にはサンドボックス化や機能制限によるセキュリティ向上のための機能と、セキュリティ機能有効化状況の評価機能が備わっている。それらを有効に活用することで、一般のサービスはもちろん、あまり使われていないセキュリティ的に多少不安があるサービスの運用リスクもある程度緩和できるだろう。

なお、昨今は OCI、主に Docker によるサービス管理も主流になりつつあり、systemd でサービスを運用する機会もなくなりつつあるかもしれない。ただ、 Docker などでも事情は同じで、サンドボックス化の強化や機能制限によるセキュリティ向上はやっておいた方がいいだろうし、そうなった時バックにあるものはほぼ systemd のバックで使われてるものと同じになってくる。逆に言えば今回紹介した機能と同じような機能が Docker 側にも用意されている。Docker の場合はある程度サンドボックス化がデフォルトでなされた状態と言えなくもないが、隔離されたコンテナの中に閉じた攻撃は可能であり、また特権利用によるホストの攻撃などの事例もいくつか上がっている。追加の保護施策は有用であり、今回紹介した項目は応用できるだろう。

ここら辺はインターネットに晒されるよく分からない有象無象を動かす人が多くなった時代としては、割と有用だと思うんだが、世間にはあまり知られてなさそうだなあと感じる。 ``systemd-analyze security`` でスコア改善するのは割とゲーム感覚でできておすすめなので、まみんなもぜひやってみてくれという感じ。では、今回はこれで。
