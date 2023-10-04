Firefish のインストール方法
=====================================

:tags: Fediverse, ActivityPub, Web, Firefish
:category: 環境構築

Fediverse は ActivityPub により接続されたサーバ群によるネットワークである。ActivityPub は短文投稿、画像・動画共有、ファイル共有などを可能とする、HTTP 上でコンテンツ共有・発信を可能とするソーシャルネットワークプロトコルだ。W3C 勧告であることもあって今や多くの実装が存在しサーバも世界中に多数立っている。

さて今回はその中で、Firefish という実装についての紹介と、自分で作っている Debian 向けの Firefish パッケージの導入方法について宣伝も兼ねて説明していく。なお、これは現時点での話で、Fediverse はかなり栄枯盛衰が激しいので、1年後には役に立たなくなってるかもしれない。ま、あくまで参考程度にしてくれって感じ。

なお、この記事は

* `Fediverse (4) Advent Calendar 2023 <https://adventar.org/calendars/8812>`_
* `Firefish Advent Calendar 2023 <https://firefish.bloggy.naskya.net/>`_

の6日目掲載記事となる。他の記事も Fediverse に関しての生の体験満載となっていて、色々面白いと思うので、ぜひ覗いてみて欲しい。

他の実装との比較
--------------------

ActivityPub 対応のサーバは結構最近多いが、僕の周りで主流なのは以下:

Mastodon: https://joinmastodon.org/
    定番サーバ。AGPLv3 の OSS。サーバサイドはメインは Rails、ストリームサーバだけ Node.js 上で Express で書かれてる。Web クライアント側は React で書かれてる。クライアント側の事実上の API 標準になっているぐらいには、利用率が高い気がする。結構思想が強めで、分散 SNS の急先鋒という感じ。良くも悪くも機能のバランスは良く、安定性も高い印象はある。

Misskey: https://misskey-hub.net/
    もう一つの定番サーバ。AGPLv3 の OSS。サーバサイドは Nest.js、Web クライアントは Vue で書かれてる。思想より、結構機能重視みたいなところがあり、いろいろ面白い機能を試しては載せていってるところに、マストドンなどにはない面白さを感じてるユーザが多そうだ。ただ、開発コミュニティが結構日本寄りということがあって、海外での利用率は低め。また、安定性も少し落ちる印象がある。それから、Mastodon API との互換がないため Mastodon 用アプリが使えなく、また Misskey 用のアプリもあまりないという面もあり、Web UI を利用してるユーザが多い。

Pleroma: https://docs-develop.pleroma.social/
    個人サーバ立てる時人気のサーバ。AGPLv3 の OSS で、サーバサイドは Phoenix、Web クライアントは Vue で書かれてる。他のサーバに比べて軽いというのと、Web クライアントとサーバサイド実装が完全に分かれており、クライアント実装もいくつかあってカスタマイズしやすいこと、Mastodon API とある程度互換がありアプリが流用しやすいことが特徴的。ただ、大型サーバではあまり用いられてるのを見ない気がする。

Akkoma: https://akkoma.social/
    Pleroma のフォーク。あまりよく知らないが、時折使ってる人はいそう。Pleroma にない機能が載ってたり、逆に Pleroma にはあるけど Akkoma にはない機能もあったりするみたいな噂。

Firefish: https://joinfirefish.org/
    Misskey のフォークで、AGPLv3 の OSS。フォーク当時は Calckey という名前だったが、改名された。今回紹介するソフトウェアで、僕の個人鯖で使っている。

なお、これらは主に短文投稿 SNS 用途という感じだが、他に WordPress のプラグイン経由での発信、PeerTube、Lemmy などの短文投稿以外に特化したソフトウェアなどもある程度シェアを占めているようだ。

これらの中で Firefish の立ち位置は、Mastodon と Misskey の中間のような感じで、Misskey の便利な機能を取り入れつつ、Mastodon API も搭載していて Mastodon 用のクライアントアプリを使える [#support-status-mastodon-api-of-firefish]_ のが特徴という感じになる。個人的に重宝してるのは、Web UI の見やすさとカスタム絵文字だろうか。UI でいうと、具体的には分散 SNS らしく発信元のソフトウェア情報が簡単に確認できるため、相手がどういう機能に対応してるソフトを使ってるか反応する前に予測できたり、設定から好みに合わせて UI 調整できたりって感じ。

僕は Fediverse に来てから、 `Vivaldi Social <https://social.vivaldi.net>`_ という Vivaldi が運営する Mastodon 鯖にしばらくいた後、Mastodon でおひとり様自己運営鯖を立ててそこに移り、その後 Firefish にソフトウェア変更を行なって3ヶ月経ったぐらいという感じの変遷をしてる。そこまでまともに比較してないのだけど、Mastodon から Firefish に切り替えた理由は、Mastodon はサービスが3つありおひとり様としては管理コストが大きかった [#maintenance-cost-of-mastodon]_ のと、Ruby、Node.js 双方のメンテが必要で、メモリも結構サービス自体が食ってくるというのが不満で、そこが改善されてる点で選んでるという感じ。Firefish は Node.js サーバ一つの管理をすればよく、メモリも Mastodon 時代は3サービスそれぞれがそれなりに食ってる状況から大体メモリ消費量半減で管理できるようになった。ただし、ジョブキューの並列数を減らしたりもしたのでここは単純な比較ができなくて眉唾部分もあるかも。とりあえず、現状の Firefish 運用で僕はそれなりに満足しているという感じ。

deb の作成
----------------------

そんな Firefish だが、インストールにはちょっと難がある。Firefish は一部サーバライブラリを Rust 化しているのだが、このビルドにかなりリソースが必要になる。一部インストール方法の紹介では、ビルド用に CPU リソースを増やしましょうなどと書かれているぐらいだ。さらにこのビルドが遅いため、オンプレ直置き、直ビルドで運用している鯖ではアップデートでの停止時間がそれなりに必要となる。Docker を使えばこの点は解決するが、Docker はそれなりにオーバーヘッドがかかり、運用にも手間がかかる。おひとり様としては、ある程度ルーズに、消費リソースも抑えつつ、ソース管理はいい感じの機構に任せたい。

そこで、僕は deb パッケージを GitHub Actions で作成し、ソースインストール・更新などは Debian 上で apt 経由で行えるようにしている。ソースは https://github.com/mizunashi-mana/firefish-dist-pkg。GitHub Actions 上で前もって npm パッケージ、Rust ソースのビルドなどを行なっておき、それを deb アーカイブにまとめておいて、鯖上には deb をダウンロードして apt 経由でインストールするという感じだ。ついでに、systemd service を付属させたりして、更新時にサービス停止と再起動が行われるようにしたり、サンドボックス機能でセキュリティを強化したり、キャッシュファイルなどの置き場をソースから切り離したりしている。

deb のパッケージ構成用ファイル群は https://github.com/mizunashi-mana/firefish-dist-pkg/tree/v1.0.5%2Brc-d202312060003%2Brelease/deb/files らへんにあって、主なファイルとしては

`control <https://github.com/mizunashi-mana/firefish-dist-pkg/blob/v1.0.5%2Brc-d202312060003%2Brelease/deb/files/control>`_
    deb パッケージのメタ情報。Firefish の依存パッケージである ffmpeg、libvips を指定して、標準リポジトリから自動でインストールされるようにしていたりする。

`rules <https://github.com/mizunashi-mana/firefish-dist-pkg/blob/v1.0.5%2Brc-d202312060003%2Brelease/deb/files/rules>`_
    deb パッケージのビルドルール。deb パッケージ作成時のタスク内容を記述している。

`service <https://github.com/mizunashi-mana/firefish-dist-pkg/blob/v1.0.5%2Brc-d202312060003%2Brelease/deb/files/service>`_
    systemd service ファイル。deb パッケージ作成時に deb パッケージに含める。サンドボックス機能をいい感じに有効化したり、書き込み可能なパスなどを制限することで、Firefish に脆弱性などがあった場合の影響を抑えられるようにしている。

みたいな感じ。ま、この辺の詳細は `Debian メンテナー用ガイド <https://www.debian.org/doc/manuals/debmake-doc/index.ja.html>`_ らへんを参照してくれ。

このパッケージをインストールすると、

``/var/lib/firefish/live``
    Firefish のソース群が置かれる。Firefish は、 ``./files`` に画像ファイルなどを置くがこのパッケージではそこを ``/var/lib/firefish/files`` へのリンクとしていて実際の書き込みはそっちに行われるため、 ``/var/lib/firefish/live`` は完全にリードオンリーでソースファイルのみが置かれる感じになる。また、Firefish は ``./.config/default.yml`` を設定ファイルとして参照するが、これも Debian の設定ファイルのディレクトリ構成に合わせて ``/etc/firefish/config.yml`` へのリンクとしていて、 ``/etc/firefish`` 以下のファイルをいじればいいようにしている。

``/var/lib/firefish/files``
    上述の通り、S3 ストレージを設定していない場合のデフォルトの画像ファイル、キャッシュファイルなど置き場。

``/var/lib/firefish/work``
    Firefish の実行ユーザのホームディレクトリで、npm などが色々ファイルを生成する場合に使われる。

``/etc/firefish``
    Firefish の設定ファイル置き場。

``/lib/systemd/system/firefish.service``
    systemd サービスファイル。上記のファイルがインストールされるところ。

などが作成される。またこれらのファイルは新しい deb ファイルをインストール際自動で更新される (設定ファイルなどは更新するか聞かれる) ようになる。実際これを使って3ヶ月ぐらい運用しているが、結構便利。更新もコマンド一発でできるし。あまり真面目にメンテはしてないが、よかったら使ってみてくれって感じ。

インストール方法
----------------------

では、deb ファイルを使う場合のオンプレ直置きでの Firefish インストール方法を、具体的に1から説明していく。今回は Debian 12 (bookworm) での説明になるが、Ubuntu でも大体同じような感じでできるはず。

Debian 12 を ISO インストーラを使ってインストールし、SSH でログインできるようにし、ファイアウォールなどは設定済みの状態を前提にする。これはそれぞれのホスティングサービスによりいい感じのやり方があると思うので、それぞれ調べて欲しいが、 `Mastodon のマシン設定ガイド <https://docs.joinmastodon.org/admin/prerequisites/>`_ らへんも参考にするのがいいんじゃないだろうか。僕の場合は、さくら VPS で Debian 12 の標準イメージを使い Debian をインストールした後、

1. SSH 用の公開鍵を入れて、SSH サーバで攻撃トラフィック低減のためのポート変更と Pubkey 認証以外の無効化を実行。
2. fail2ban を入れ、sshd 用の設定を有効化。
3. nftables でファイアウォール設定。

をしている。nftables で `Mastodon のマシン設定ガイド`_ と同等のものを設定するには、

::

    #!/usr/sbin/nft -f

    flush ruleset

    table inet filter {
        chain input {
            type filter hook input priority 0;

            # The default policy of inbound is dropping.
            policy drop;

            # Allow all loopback (lo0) traffic and drop all traffic to local addresses that doesn't use lo0
            iif lo accept;
            ip saddr 127.0.0.0/8 iif != lo reject;
            ip6 saddr ::1/128 iif != lo reject;

            # Accept all established inbound connections
            ct state { established, related } accept;

            # Allow HTTP and HTTPS connections from anywhere (the normal ports for websites and SSL).
            tcp dport { 80, 443 } accept;

            # Allow SSH connections
            # The -dport number should be the same port number you set in sshd_config
            ct state new tcp dport 22 accept;

            # Allow ping
            icmp type echo-request accept;
            ip protocol ipv6-icmp accept;

            # Allow destination unreachable messages, especially code 4 (fragmentation required) is required or PMTUD breaks
            icmp type destination-unreachable accept;

            # Log iptables denied calls
            limit rate 5/minute burst 5 packets log prefix "nftables denied: " level debug;
        }

        chain forward {
            type filter hook forward priority 0;

            # The default policy of forward is dropping.
            policy drop;
        }

        chain output {
            type filter hook output priority 0;

            # Allow all outbound traffic - you can modify this to only allow certain traffic
            policy accept;
        }
    }

を ``/etc/nftables.conf`` に書き込んで (SSH ポート番号 22 の部分は変更している場合は正しいポートに直す必要がある)、 ``systemctl restart nftables`` すればいいだろう。若干、icmpv6 パケット全許可だったりするのが気になるが、ひとまずはこれでいいんじゃないだろうか。Ubuntu の場合は ufw とか使うのが普通だと思うので、無理に

またドメインの取得と設定も前提にする。僕は `Cloudflare Registrar <https://www.cloudflare.com/ja-jp/products/registrar/>`_ 経由でドメインを買い、そのサブドメインに A レコードと AAA レコードを生やしている。まこの辺はいい感じのサービス見つけるなり、上級者向けだが自分で DNS サーバ立てるなりしてくれ。

その前提で、まず Node.js v20 インストール用の準備をしておく。https://github.com/nodesource/distributions#installation-instructions に沿って、インストールを進めていく。具体的には、

::

    sudo apt-get update
    sudo apt-get install -y ca-certificates curl gnupg
    sudo mkdir -p /etc/apt/keyrings
    curl -fsSL https://deb.nodesource.com/gpgkey/nodesource-repo.gpg.key | sudo gpg --dearmor -o /etc/apt/keyrings/nodesource.gpg
    NODE_MAJOR=20
    echo "deb [signed-by=/etc/apt/keyrings/nodesource.gpg] https://deb.nodesource.com/node_$NODE_MAJOR.x nodistro main" | sudo tee /etc/apt/sources.list.d/nodesource.list
    sudo apt update

を実行する [#nodesource-setup-flow]_。

次に、Firefish のデータベース用に Redis と PostgreSQL をインストールして起動しておく::

    sudo apt install -y redis-server postgresql-15
    sudo systemctl restart redis-server
    sudo systemctl restart postgresql
    sudo systemctl enable redis-server # 再起動時に自動起動するようにする
    sudo systemctl enable postgresql # 再起動時に自動起動するようにする

そして、Firefish データベース用の PostgreSQL ユーザとデータベースを作成する::

    sudo -u postgres createuser firefish-user --pwprompt
    sudo -u postgres psql -c "CREATE DATABASE firefish OWNER 'firefish-user' ENCODING 'UTF-8';"

次に、Firefish パッケージをダウンロードして、インストールする。https://github.com/mizunashi-mana/firefish-dist-pkg/releases からインストールしたいバージョン、基本は最新の自分が使っているディストリビューション用の deb ファイルをダウンロードして、apt 経由でインストールする。具体的には、https://github.com/mizunashi-mana/firefish-dist-pkg/releases/tag/v1.0.5%2Brc-d202312060003%2Brelease の Debian bookworm 用の deb をインストールする場合は

::

    curl -L https://github.com/mizunashi-mana/firefish-dist-pkg/releases/download/v1.0.5%2Brc-d202312060003%2Brelease/debian-bookworm-amd64_firefish.deb --output firefish.deb
    sudo apt install -y ./firefish.deb

すればいい。後は設定を弄って、Firefish の立ち上げを行う。設定は ``/etc/firefish/config.yml`` を編集する。まず、ドメイン部分を正しいものに修正する:

.. code-block:: diff

    - url: https://example.com/
    + url: https://<使うドメインをここに書く>/

それから PostgreSQL の設定を修正する。データベース名、ユーザ名、パスワードを先程作成したものに修正する:

.. code-block:: diff

      db:
          host: localhost
          port: 5432
          #ssl: false
          # Database name
          db: firefish

          # Auth
    -     user: example-firefish-user
    -     pass: example-firefish-password
    +     user: firefish-user
    +     pass: <先程設定したパスワードをここに書く>

          # Whether disable Caching queries
          #disableCache: true

後は適宜弄りたい設定があったらいじる。個人的に、Redis の prefix を設定しておくのがおすすめだ。これは新たに Redis を使う他のサーバが生まれた時に同居させやすいからだ。ま、そういう予定がなければ特に弄らなくてもいいだろう。ここまで出来たら、Firefish を起動する::

    sudo systemctl restart firefish
    sudo systemctl enable firefish # 再起動時に自動起動するようにする

次に Firefish の管理者ユーザを作成しておく。これは後ででもいいが、セキュリティ的にはここで作っておくのが安全だ。SSH でポートフォワーディングして、ブラウザから作成するのがおすすめだ。まずサーバ内でなく手元で以下を実行して、ポートフォワーディングを行う::

    ssh -L 3000:localhost:3000 <接続先ホスト>

これにより、http://localhost:3000 にブラウザでアクセスすると、Firefish の管理者ユーザ作成画面が立ち上がる。後はユーザ名とパスワードを入力して、管理者ユーザを作成する。このユーザで以降投稿などができるようになる。

次に、リバースプロキシの用意と TLS 化を行う。ここら辺は、レジストラサービスに付随していたりもするので、そっちを使う場合は不要になる。自前で用意する場合は、まず Nginx のインストールを行う::

    sudo apt install -y nginx

その後、以下のような設定を ``/etc/nginx/sites-available/firefish.conf`` に書き込む::

    map $http_upgrade $connection_upgrade {
        default upgrade;
        ''      close;
    }

    upstream firefish {
        server localhost:3000 fail_timeout=0;
    }

    proxy_cache_path
        /var/cache/nginx/firefish
        levels=1:2
        keys_zone=cache_firefish:16m
        inactive=720m
        max_size=1g
        use_temp_path=off
        ;

    server {
        listen 80;
        listen [::]:80;
        server_name <使うドメインをここに書く>;

        root /var/www/html/firefish;

        location /.well-known/acme-challenge/ {
            allow all;
        }

        location / {
            return 301 https://$host$request_uri;
        }
    }

    server {
        # TLS 設定をした後、以下のコメントアウトを外す
        listen 127.0.0.1:443;
        # listen 443 ssl http2;
        # listen [::]:443 ssl http2;
        # ssl_certificate     /path/to/fullchain.pem;
        # ssl_certificate_key /path/to/privkey.pem;

        server_name <使うドメインをここに書く>;

        # You can use https://ssl-config.mozilla.org/ to generate your cipher set.
        # We recommend their "Intermediate" level.
        ssl_protocols TLSv1.2 TLSv1.3;
        ssl_ciphers ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-CHACHA20-POLY1305:ECDHE-RSA-CHACHA20-POLY1305:DHE-RSA-AES128-GCM-SHA256:DHE-RSA-AES256-GCM-SHA384:DHE-RSA-CHACHA20-POLY1305;
        ssl_prefer_server_ciphers on;
        ssl_session_cache shared:SSL:10m;
        ssl_session_tickets off;

        keepalive_timeout    70;
        sendfile             on;
        client_max_body_size 99m;

        gzip on;
        gzip_disable "msie6";
        gzip_vary on;
        gzip_proxied any;
        gzip_comp_level 6;
        gzip_buffers 16 8k;
        gzip_http_version 1.1;
        gzip_types text/plain text/css application/json application/javascript text/xml application/xml application/xml+rss text/javascript image/svg+xml image/x-icon;

        proxy_redirect off;
        proxy_http_version 1.1;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;

        # For WebSocket
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection $connection_upgrade;

        location / {
            proxy_pass http://firefish;

            # Cache settings
            proxy_cache cache_firefish;
            proxy_cache_lock on;
            proxy_cache_use_stale error timeout updating http_500 http_502 http_503 http_504;
            add_header X-Cache $upstream_cache_status;
        }
    }

それから、キャッシュ用のディレクトリ作成と、Nginx 設定の有効化を行う::

    sudo mkdir -p /var/cache/nginx
    sudo chown www-data:www-data /var/cache/nginx
    sudo ln -s /etc/nginx/sites-available/firefish.conf /etc/nginx/sites-enabled/firefish.conf
    sudo rm -rf /etc/nginx/sites-enabled/default # デフォルトの設定を無効化
    sudo systemctl restart nginx
    sudo systemctl enable nginx # 再起動時に自動起動するようにする

次に TLS 証明書をインストールする。証明書をどっかで買ってる場合は、それをダウンロードしてくる。証明書に拘りがなく、とりあえず TLS 化だけしたい場合は、Let's Encrypt を利用するといいだろう。ここでは、Let's Encrypt を使う場合の設定を説明する。まず、certbot をインストールして、証明書発行を行う::

    sudo apt install -y certbot python3-certbot-nginx
    sudo certbot certonly --nginx -d <使うドメインをここに書く> --post-hook "systemctl reload nginx"
    sudo systemctl enable certbot.timer # 証明書の自動更新を有効化する

TLS 証明書をダウンロードしたら、Nginx の設定ファイルを修正して、TLS 証明書を読み込むようにする:

.. code-block:: diff

    -     # TLS 設定をした後、以下のコメントアウトを外す
    -     listen 127.0.0.1:443;
    -     # listen 443 ssl http2;
    -     # listen [::]:443 ssl http2;
    -     # ssl_certificate     /path/to/fullchain.pem;
    -     # ssl_certificate_key /path/to/privkey.pem;
    +     listen 443 ssl http2;
    +     listen [::]:443 ssl http2;
    +     ssl_certificate     /etc/letsencrypt/live/<使うドメインをここに書く>/fullchain.pem;
    +     ssl_certificate_key /etc/letsencrypt/live/<使うドメインをここに書く>/privkey.pem;

上記は Let's Encrypt の証明書を使う場合だが、独自の証明書を使う場合はその証明書の秘密鍵と中間証明書のパスを設定するようにする。そして、Nginx を再起動する::

    sudo systemctl restart nginx

ここまで上手くいったら、ブラウザで https://<使うドメイン>/ にアクセスして、Firefish の画面が表示されることを確認する。これで、Firefish のインストールは完了となるはず。後は、更新があったらまた deb をダウンロードしてきて、 ``sudo apt install -y ./firefish.deb`` で更新できる。

うまく行かなかったら、その時はまあ頑張ってくれ。 `鯖缶工場 <https://wiki.sabakan.industries/>`_ という分散 SNS のサーバ管理者の寄合所帯があるので、そこに助け求めると誰かが助けてくれるかもしれない。

まとめ
------------

というわけで、Firefish の紹介と、そのインストール方法を紹介した。多分、あまり Firefish を直でインストールする系の記事はないと思うので、それを放流しておくのも兼ねてという感じ。何かの参考になれば。

この先、deb 配布を真面目にメンテしていくかは分からないが、ひとまず自分で使ってるので僕が Firefish ユーザを辞めない限りは続けていくんじゃないだろうか。何か要望があれば、言って貰えばすぐ対応できるものなら対応するかもしれない。というわけで、今回はそんな感じで。

.. [#support-status-mastodon-api-of-firefish] 使えると言っても若干更新が追いついていなかったり、対応が不完全だったりするので、完全に Mastodon 用のクライアントアプリが使えるというわけではない。また、Mastodon 用のクライアントアプリを使う場合、Firefish 用の機能を使えない場合があるので注意が必要。
.. [#maintenance-cost-of-mastodon] 逆に大手だとそれぞれでスケーリングできるので、サービス分かれてる方が嬉しいかもしれない。
.. [#nodesource-setup-flow] たまにインストール方法が変わってることがあるので、nodesource の README の方をまずは参照して欲しい。
