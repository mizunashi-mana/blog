アーカイブ済み ActivityPub サーバの実装
===============================================

:tags: Fediverse, ActivityPub, Web, アーカイブ
:category: サービス

Fediverse は ActivityPub をしゃべるサーバを基本単位とした分散型 SNS ネットワークである。ActivityPub は W3C が勧告する分散型 SNS の標準プロトコルで、ActivityStreams というデータフォーマットプロトコルを元に、SNS 上のコンテンツを HTTP 上で JSON を通してやり取りする通信方法を規定する。サーバ実装も多様化しているが、基本的には ActivityStreams の方に拡張を加えた ActivitiyPub と、WebFinger と呼ばれるアカウントに対して紐づけるリソース URL を HTTP で発信するプロトコルへの対応、そしてクライアント用 API を用意しているものが多い。

さて、分散型 SNS は変化が早く、各サーバの寿命も短い傾向にある。運営の完全撤退の場合もあるが、サーバソフトウェアの変更や、並走して動かしていたサーバをどちらかに統一したい場合など、管理上の都合で一部サーバを停止することもよくある。しかしそうなった場合、リンク先が機能しなくなり引用文献などが参照できなくなる他、ユーザIDから情報が辿れなくなってしまい参照性が失われやすい。

丁度、僕も Mastodon から Firefish というソフトウェアへの移行を目指しており、最近、幾つか Fediverse インスタンスのアーカイブ方法について研究してみていた。今回は Fediverse の技術的な概要と研究した内容について紹介したいと思う。

Fediverse を支えるプロトコル
-----------------------------------

Fediverse で基本となるプロトコルは、ActivityPub と呼ばれるものである。ActivityPub では、actor と呼ばれるリソースから activity と呼ばれるメッセージを配信したり、逆に actor が activity を受信することで、Fediverse 上のコンテンツをやり取りする。actor はメッセージ受信用リソース inbox とメッセージ送信用リソース outbox を持っており、これらのリソースから HTTP GET メソッドでメッセージを取得、POST メソッドでメッセージを送信する。 `W3C の ActivityPub recommendation <https://www.w3.org/TR/activitypub/>`_ から図を借りると、

.. image:: {attach}archived-activitypub-server/activitypub-tutorial-2.png
   :alt: ActivityPub チュートリアル

のようなイメージとなる。

例えば、Mastodon の公式アカウントの actor リソースは、 https://mastodon.social/users/Mastodon という URI に置かれている。アクセスするには、

::

    $ curl -qsSL 'https://mastodon.social/users/mastodon' -H 'Accept: application/activity+json' | jq
    {
        "@context": [
            "https://www.w3.org/ns/activitystreams",
            ..
        ],
        "id": "https://mastodon.social/users/Mastodon",
        "type": "Person",
        "following": "https://mastodon.social/users/Mastodon/following",
        "followers": "https://mastodon.social/users/Mastodon/followers",
        "inbox": "https://mastodon.social/users/Mastodon/inbox",
        "outbox": "https://mastodon.social/users/Mastodon/outbox",
        "featured": "https://mastodon.social/users/Mastodon/collections/featured",
        "featuredTags": "https://mastodon.social/users/Mastodon/collections/tags",
        "preferredUsername": "Mastodon",
        "name": "Mastodon",
        "summary": "<p>Free, open-source decentralized social media platform.</p>",
        "url": "https://mastodon.social/@Mastodon",
        ...
    }

のように、 ``Accept`` ヘッダに ``application/activity+json`` をつけて HTTP GET メソッドでアクセスを行う必要がある。このように、ActivityPub では基本的に `JSON-LD <https://www.w3.org/TR/json-ld/>`_ と呼ばれる JSON 形式の上で構造化データを表現する形式を採用しており、MIMEタイプ ``application/activity+json`` のデータとして Web 上でやり取りを行う。ただ、ActivityPub 自体はそのデータをどうやり取りするかを主に定義しており、データの意味論や形式の標準は `ActivityStreams <https://www.w3.org/TR/activitystreams-core/>`_ に切り出されている。actor リソースが持つ基本的な情報は、以下の通り:

inbox リソース
    inbox リソースは actor が受け取った activity のコレクションを表すリソース。ActivityPub では、actor の ``inbox`` フィールドにその URI が指定されているのが基本的な制約になる。inbox リソースはクライアント向けに GET メソッドでアクセスされると activity コレクションを返し、連合サーバからは POST メソッドで activity を受け取るようになっている必要がある。ただ、GET メソッドの方は対応してるソフトはあんまりないが。なお、連合を拒否するには POST に対して 405 を返すのがいいらしい [#activitypub-inbox-delivery]_。

outbox リソース
    outbox リソースは actor が発信してる activity のコレクションを表すリソース。ActivityPub では、actor の ``outbox`` フィールドにその URI が指定されているのが基本的な制約になる。outbox リソースは連合サーバ向けに GET メソッドでアクセスされると activity コレクションを返し、クライアントからは POST メソッドで配信する activity を受け取るようになっている必要がある。ただ、こちらはあまり実際に使ってるソフトはいなくて対応状況も悪い。まあ、GET メソッドの方は大体対応している気はするが。

followers リソース
    該当の actor をフォローしている actor のコレクションを表すリソース。ActivityPub では、actor の ``followers`` フィールドにその URI が指定されているのが基本的な制約になる。

following リソース
    該当の actor がフォローしている actor のコレクションを表すリソース。ActivityPub では、actor の ``following`` フィールドにその URI が指定されているのが基本的な制約になる。

他にも作成日、名前、アイコン画像などが載せられるが、まあその辺の詳細は `ActivityStreams の規格 <https://www.w3.org/TR/activitystreams-core/#actors>`_ を覗いてもらうのがいいだろう。他に、Mastodon や Misskey などの各ソフトウェアで独自の拡張も加えていたりする。例えば、Mastodon にはディレクトリ掲載の可否を示す ``discoverable`` というフラグを追加していたり、Misskey では猫かどうかを示す ``isCat`` というフラグが追加されていたりする。Mastodon の拡張は https://docs.joinmastodon.org/spec/activitypub/ に、Misskey の拡張は https://misskey-hub.net/ns.html にそれぞれ記載されているので、参考にしてみるのがいいだろう。

この actor が持っている inbox、outbox にデータを送り合うことで、Fediverse 上でのコンテンツ共有が行われる。基本的には、連合してるサーバ同士で発信するデータを互いに送信先に inbox に POST で突っ込むのが通例だ。なお、あまり使われてなさそうだが、outbox リソースを定期的に監視することでもデータを追えはする。なので実は ActivityPub では push 型、つまり送信したいデータを送信先に投入するだけでなく、pull 型、受信側が送信元を定期的に監視することでもデータを受信できる。この際配信される情報は2つある。1つはコンテンツそのもの、もう1つがそのコンテンツに対する操作、例えばコンテンツ作成や編集、削除などである。この2つを合わせたものが activity と呼ばれ、この activity をやり取りすることでコンテンツ共有が行われる。例えば、Mastodon iOS アプリがリリースされた旨の投稿作成 activity を見てみると、

::

    $ curl -qsSL 'https://mastodon.social/users/Mastodon/statuses/109831774267343989/activity' | jq
    {
        "@context": [
            "https://www.w3.org/ns/activitystreams",
            ...
        ],
        "id": "https://mastodon.social/users/Mastodon/statuses/109831774267343989/activity",
        "type": "Create",
        "actor": "https://mastodon.social/users/Mastodon",
        "published": "2023-02-08T23:44:35Z",
        "to": [
            "https://www.w3.org/ns/activitystreams#Public"
        ],
        "cc": [
            "https://mastodon.social/users/Mastodon/followers"
        ],
        "object": {
            "id": "https://mastodon.social/users/Mastodon/statuses/109831774267343989",
            "type": "Note",
            "summary": null,
            "inReplyTo": null,
            "published": "2023-02-08T23:44:35Z",
            "url": "https://mastodon.social/@Mastodon/109831774267343989",
            ...
            "content": "<p>Today we&#39;ve released a new update to our iOS app! It brings an improved sign-up flow, home screen widgets, and many accessibility improvements.</p><p><a href=\"https://apps.apple.com/us/app/mastodon-for-iphone-and-ipad/id1571998974\" target=\"_blank\" rel=\"nofollow noopener noreferrer\" translate=\"no\"><span class=\"invisible\">https://</span><span class=\"ellipsis\">apps.apple.com/us/app/mastodon</span><span class=\"invisible\">-for-iphone-and-ipad/id1571998974</span></a></p>",
            ...
        }
    }

といった感じだ。activity リソースが持つ基本的な情報は、以下:

種別
    ``Create``、 ``Update``、 ``Delete``、 ``Follow`` などのコンテンツに対する操作の種別。activity の ``type`` フィールドに指定する。

コンテンツ
    操作対象のオブジェクト。 ``Note``、 ``Image``、 ``Audio`` など色々な種別を持つ。activity の ``object`` フィールドに指定する。

配信先
    activity の配信先。actor のリソース URI を指定できる他、https://www.w3.org/ns/activitystreams#Public のような特別な配信先の URI などが指定できる。配信形式によって activity の ``to``、 ``bto``、 ``cc``、 ``bcc``、 ``audience`` フィールドに指定する。

こんな感じの情報を Fediverse では送り合ってるわけだ。まあ、送受信はもう少し色々面倒があるんだが、今回はそこには立ち入らない。

さて、このように actor リソースを中心に、ActivityPub は回ってる。ただ、問題はこの actor リソースの場所を共有する方法である。URI を直接共有してもいいが、Fediverse ではアカウント ID を別途発行し、それを元に actor リソースを特定する方法が取られている。これは、アカウントリソースを ActivityPub 前提にしないためだろう。このアカウント ID から actor リソースの特定方法を提供するプロトコルが WebFinger と呼ばれるものになる。WebFinger は、 ``/.well-known/webfinger`` という URI に、クエリパラメータ付きで HTTP GET アクセスをすると、そのパラメータにあったリソース URI を JSON 形式で返してくるという割と単純なもの。例えば、 ``@Mastodon@mastodon.social`` のリソースをクエリするには、

::

    $ curl 'https://mastodon.social/.well-known/webfinger?resource=acct:Mastodon@mastodon.social'
    {
        "subject": "acct:Mastodon@mastodon.social",
        "aliases": [
            "https://mastodon.social/@Mastodon",
            "https://mastodon.social/users/Mastodon"
        ],
        "links": [
            {
                "rel": "http://webfinger.net/rel/profile-page",
                "type": "text/html",
                "href": "https://mastodon.social/@Mastodon"
            },
            {
                "rel": "self",
                "type": "application/activity+json",
                "href": "https://mastodon.social/users/Mastodon"
            },
            {
                "rel": "http://ostatus.org/schema/1.0/subscribe",
                "template": "https://mastodon.social/authorize_interaction?uri={uri}"
            },
            {
                "rel": "http://webfinger.net/rel/avatar",
                "type": "image/png",
                "href": "https://files.mastodon.social/accounts/avatars/000/013/179/original/b4ceb19c9c54ec7e.png"
            }
        ]
    }

のようにする。WebFinger では

``resource``
    WebFinger リソースの URI。Fediverse では、 `acct URI スキーム <https://datatracker.ietf.org/doc/html/rfc7565>`_ が使われることが多い。actor リソース URI を直接使える実装も多い。

``rel``
    オプションで、リソース URI の制限を指定できる。

のパラメータを受け取り、MIMEタイプ ``application/jrd+json`` の JSON データを返す。レスポンスは cross origin でも受け付けられるよう、 ``Access-Control-Allow-Origin: *`` をつけることが要求されていて、

``subject``
    WebFinger リソースの一意な URI。Fediverse では基本 `acct URI スキーム <https://datatracker.ietf.org/doc/html/rfc7565>`_ が使われる。

``aliases``
    オプションで指定される、他にリソースを特定するために使える URI。

``properties``
    オプションで指定される、付加情報。

``links``
    WebFinger リソースに紐づくリソースへのリンク。Fediverse では、HTML リソース、ActivityPub actor リソース、OStatus と呼ばれる ActivityPub の前身の購読リソース、アバター画像のリソースなどがリンクされてることが多い。

などが返ってくる。この WebFinger は各 ActivityPub 実装に付随している。Fediverse 上の UI では、アカウント ID のドメインに対してクエリがかけられ、上記情報から actor リソースを特定し、actor リソースからアカウント情報を引っ張ってくるという感じになる。

静的な ActivityPub サーバ
--------------------------------

さて、ActivityPub のコンテンツ共有を除き、WebFinger から actor リソースの特定を行うまでは、単純な GET メソッドの連鎖によって行うことができる。実際、Mastodon や Misskey などに、擬似的にアカウントを作って認識させるだけなら、静的なサイトで十分可能だ。もちろん、これだと POST メソッドで inbox に activity を投入することなどはできないため、コンテンツ配信はできないが、逆に言えばコンテンツ配信しないサーバ、例えばアーカイブ済みのサーバを建てることはできる。これによりバックエンドをオブジェクトストレージにすることなども視野に入れることができ、サーバ運営の費用と手間を抑えることも期待できる。

ま、実際に見てみるのが早いと思うので、Cloudflare Pages でちょっと規格から外れる部分はあるが、Mastodon や Misskey で問題なく認識される擬似的な ActivityPub サーバを立ててみる。この擬似的なサーバはアカウントを一つしか持つことができない。これは、WebFinger がクエリパラメータによりリソースを認識するという問題によるものだ。ここに関してはリダイレクタを挟むなり、今回のようにクエリパラメータガン無視で一意なリソースを返すなどの工夫をする必要がある。その制約さえ許容すれば Cloudflare Pages で擬似的なサーバを立てられる。

まず、特別なリソースをいくつか用意しておく:

::

    $ cat static/empty-collection.json
    {
        "@context": "https://www.w3.org/ns/activitystreams",
        "id": "https://virtual-ap-demo-mizunashi-work.pages.dev/static/empty-collection.json",
        "type": "Collection",
        "totalItems": 0,
        "items": []
    }
    $ cat static/empty-ordered-collection.json
    {
        "@context": "https://www.w3.org/ns/activitystreams",
        "id": "https://virtual-ap-demo-mizunashi-work.pages.dev/static/empty-ordered-collection.json",
        "type": "OrderedCollection",
        "totalItems": 0,
        "orderedItems": []
    }

これは空のコレクションを表すリソース。とりあえず、 actor が認識されればいいので、 ``inbox``、 ``outbox``、 ``following``、 ``followers`` など ActivityPub で必要とされるコレクションリソースには適宜上記のリソースを埋めておく。その為のもの。今回は、 ``virtual-ap-demo-mizunashi-work`` というプロジェクト名で Cloudflare Pages にプロジェクトを作ったので、それに合わせた ``id`` を指定する。その辺は試す環境に合わせて変えるのがいいだろう。次にこれを元に actor リソースを作る:

.. code-block:: json

    {
        "@context": [
            "https://www.w3.org/ns/activitystreams",
            {
                "manuallyApprovesFollowers": "as:manuallyApprovesFollowers",
                "alsoKnownAs": {
                    "@id": "as:alsoKnownAs",
                    "@type": "@id"
                },
                "movedTo": {
                    "@id": "as:movedTo",
                    "@type": "@id"
                }
            },
            {
                "toot": "http://joinmastodon.org/ns#",
                "featured": {
                    "@id": "toot:featured",
                    "@type": "@id"
                },
                "featuredTags": {
                    "@id": "toot:featuredTags",
                    "@type": "@id"
                },
                "discoverable": "toot:discoverable",
                "devices": {
                    "@type": "@id",
                    "@id": "toot:devices"
                },
                "suspended": "toot:suspended"
            }
        ],
        "id": "https://virtual-ap-demo-mizunashi-work.pages.dev/static/users/virtual-acct.json",
        "type": "Person",
        "following": "https://virtual-ap-demo-mizunashi-work.pages.dev/static/empty-ordered-collection.json",
        "followers": "https://virtual-ap-demo-mizunashi-work.pages.dev/static/empty-ordered-collection.json",
        "inbox": "https://virtual-ap-demo-mizunashi-work.pages.dev/static/not-found.json",
        "outbox": "https://virtual-ap-demo-mizunashi-work.pages.dev/static/empty-ordered-collection.json",
        "featured": "https://virtual-ap-demo-mizunashi-work.pages.dev/static/empty-ordered-collection.json",
        "featuredTags": "https://virtual-ap-demo-mizunashi-work.pages.dev/static/empty-collection.json",
        "preferredUsername": "virtual-acct",
        "name": "Virtual User",
        "summary": "A virtual user of @mizunashi_mana@mstdn.mizunashi.work. Anyone cannot follow me.",
        "url": "https://virtual-ap-demo-mizunashi-work.pages.dev/index.html",
        "manuallyApprovesFollowers": false,
        "discoverable": true,
        "published": "2023-08-07T00:00:00Z",
        "devices": "https://virtual-ap-demo-mizunashi-work.pages.dev/static/empty-collection.json",
        "movedTo": "https://mstdn.mizunashi.work/users/mizunashi_mana",
        "tag": [],
        "attachment": []
    }

これを ``static/users/virtual-acct.json`` においておく。さらに、 ``.well-known/webfinger`` に、

.. code-block:: json

    {
        "subject": "acct:virtual-acct@virtual-ap-demo-mizunashi-work.pages.dev",
        "aliases": [
            "https://virtual-ap-demo-mizunashi-work.pages.dev/index.html",
            "https://virtual-ap-demo-mizunashi-work.pages.dev/static/users/virtual-acct.json"
        ],
        "links": [
            {
                "rel": "http://webfinger.net/rel/profile-page",
                "type": "text/html",
                "href": "https://virtual-ap-demo-mizunashi-work.pages.dev/index.html"
            },
            {
                "rel": "self",
                "type": "application/activity+json",
                "href": "https://virtual-ap-demo-mizunashi-work.pages.dev/static/users/virtual-acct.json"
            }
        ]
    }

という JSON ファイルをおく。後は、 ``index.html`` を内容適当で作っておいておき、GitHub にアップして、Cloudflare Pages の GitHub connector で接続してプロジェクトを作って ``@virtual-acct@virtual-ap-demo-mizunashi-work.pages.dev`` で検索すると、 ``@mizunashi_mana@mstdn.mizunashi.work`` に引越し済みのユーザを確認することができるだろう。コードの全貌は https://github.com/mizunashi-mana/virtual-ap-demo.mizunashi.work において実際にデプロイしてあるのでぜひ試してほしい。

Fediverse 上のソフトはこのアカウント ID を次の手順で辿ることが期待される:

1. まず、 ``https://virtual-ap-demo-mizunashi-work.pages.dev/.well-known/webfinger?resource=acct:virtual-ap-demo-mizunashi-work.pages.dev`` にアクセスして、WebFinger リソースを取得し、 ``rel=self`` の actor リソース URI ``https://virtual-ap-demo-mizunashi-work.pages.dev/static/users/virtual-acct.json`` を入手する。
2. 次に、 ``https://virtual-ap-demo-mizunashi-work.pages.dev/static/users/virtual-acct.json`` にアクセスして actor リソースを入手する。この際、適宜 ``following``、 ``followers`` フィールドから空のコレクションリソースにアクセスしたり、 ``movedTo`` からこの actor が引越し済みで引越し先は ``https://mstdn.mizunashi.work/users/mizunashi_mana`` であることなどを知る。
3. 必要な情報を自身のデータベースに登録し、ユーザ情報を検索結果として表示する。

ま、多少複雑だが、こんぐらいできれば引越し済みアカウントは別にネット世界から抹消しなくても残せるというわけだ。

archivedon
---------------

さて、上記のようにリソースをリンクだけ書き換えて単に JSON ファイル化して、いい感じに配置していけば、配信しない ActivityPub サーバは作れる。ただ、どうせなら、旧 URL からの参照性を残したり、WebFinger でマルチアカウント対応ぐらいはできるようにしておきたい。なので、そこら辺だけはうまくできるように、後アーカイブ済みリソースを閉じる予定のサーバから簡単に作れるようなソフトを Rust で書いてみた。コードは、 https://github.com/mizunashi-mana/archivedon にある。

ここまでの話を踏まえれば、やってることはそこまで難しくない。このサーバ、archivedon が提供する機能は大きく3つ:

static リソースファイルのサービング
    これは単純に指定のデータディレクトリの ``./static`` をそのままサーブするだけ。ここだけ、どっかのオブジェクトストレージでサービングさせることも可能。内容をどう配置するかは後述する。

WebFinger、nodeinfo などの情報生成
    WebFinger リソースについては上記の通り。resource、rel パラメータを受け取り、いい感じの JSON を返す。内容はデータディレクトリの ``./webfinger`` ディレクトリから取得する。内容をどう配置するかは後述する。後、Misskey や Firefish などのソフトでは、nodeinfo というリソースにも対応しておくと、サーバ情報をそこから取得していい感じに付加情報として表示してくれる。これも生成するようにしている。

旧 URI からのリダイレクト
    指定のデータディレクトリの ``./map`` ディレクトリにリダイレクト情報がある場合は、それを元にリダイレクトを行う。内容をどう配置するかは後述する。

後は、Misskey、Firefish 辺りはホームページのメタ情報などもスクレイピングしてきたりするので、トップページの HTML サービング機能などもつけている。使い方は、https://github.com/mizunashi-mana/archivedon/releases からバイナリ落としてきて、

::

    archivedon serve --port 3333 --resource-dir ./resource --expose-url-base https://archivedon.mizunashi.work/

みたいな感じでいい感じに ``resource`` ディレクトリ作って、いい感じにドメイン発行とかして、いい感じに downstream 設定すれば立つ。ま、静的ファイルサービングするだけの HTTP サーバなので、データベースとかもいらんし、キャッシュとかは nginx とか downstream 側でよしなにやってくれという感じ。static リソースも基本オブジェクトストレージに載せられるなら、downstream 側でそっちに振り分ければ archivedon は well known リソースのサービングとリダイレクタとしての役割だけでよくなり、さらにメンテが楽になるだろう。

それから、サーバがサーブするリソースを、稼働している ActivityPub サーバからスクレイプして作る機能も載せている。データベースからさらって作る方が効率は良いが、それだと結構公開リソースかどうかの判別で事故りそうだったのと、かなり実装依存になりそうだったのであえて避けた。代わりに HTTP リクエストで fetch しまくるので、リソースファイルの作成がかなり遅く、サーバ側にも負荷がかかる。あまり大規模なサーバではお勧めできない。やることは、

.. code-block:: json

    {
        "static_base_url": "https://archivedon.mizunashi.work/static/",
        "accounts": [
            "@mizunashi_mana@mstdn.mizunashi.work"
        ]
    }

みたいな JSON ファイルを ``input.json`` として作り、

::

    archivedon-fetch --input input.json --output resource --fetch-outbox

みたいな感じで実行すると、 ``accounts`` で指定されたアカウントIDから、WebFinger を経由して actor リソースを取得し、inbox、outbox などの諸々を空のコレクションリソースに、他も書き換えが必要なところは書き換えつつ、必要ないところはそのまま残しつつでリソースファイルを作成して ``./resource/static`` 以下においたり、 ``./resource/map`` にリダイレクトマップを作ったり、 ``./resource/webfinger`` に WebFinger リソースを作ったりしてくれる。後、 ``--fetch-outbox`` を指定しておくと、同じ要領で outbox の activity を辿り投稿リソースなどに対しても同じようなことをしてくれる。後はまあ、修正することがあったら、単なる JSON ファイルなので手動で手を加えたりすることもできるだろうという感じ。

Content-Type などが多少規格から外れてる部分があるのと、WebFinger が受け取れるパラメータが acct URI にしか対応してないみたいな問題はあるが、実用上はそこまで問題ないんじゃないかなという感じ。ひとまず Mastodon、Firefish などでは認識されるよう頑張った。

まとめ
---------

というわけで、Fediverse を支えるプロトコルの、主に情報取得部分特化での簡単な紹介と、その部分を利用して割りかし静的なアーカイブ済みサーバ専用ソフトの仕組み紹介をした。ひとまずこれ使ってアーカイブしていきつつ、Mastodon から Firefish へ完全にお引越ししようかなという感じ。

やっぱ完全に諸々持ち越せないのが Fediverse ではちょっと辛いな、そこはもうちょっとプロトコルレベルで考慮があっても良かったのではというのはあるが、ま認証というのは色々技術的に難しい問題なのでしょうがないね。アーカイブは notestock に頼るというのも一つの手だが、notestock が滅びる可能性は十分あるし、コードが公開されてて、割りかし自分の裁量でアーカイブ物の調整がしやすいものが欲しかったので、割と満足。後、ActivityPub の諸々についてちょっと知識がついて、解像度が上がった。これを機に、Fediverse の諸々を触ってみてもいいかなと思ったりしたが、ま時間はないですね。では、今回はこれで。

.. [#activitypub-inbox-delivery] https://www.w3.org/TR/activitypub/#delivery
