nftables のログを JSON で吐く
=============================================

:tags: nftables, ファイアウォール, Debian
:category: 運用

Linux でのファイアウォールが nftables になってからかなり経つ。iptables フロントエンドよりも分かりやすい設定ファイルと設定の永続化方法も用意され、カーネル側で単純なファイアウォールを組むだけなら nftables 単体で十分になった。

さて、nftables にはパケットフィルタリングの機能に加えて、パケットのロギングをする機能が組み込まれている。これは iptables 時代からある機能で、パケットフィルタの動作監視や、パケット流入の状況監視などに使える。標準では、このログはカーネルログに独自の形式で吐かれるが、フォーマッタや出力先は調整できる機能が入っている。今回は、この機能を ulogd を通して使い、JSON 形式で別ファイルにパケットログを出す方法を紹介する。

nftables とログ出力
-------------------------------

nftables の基本的な使い方は、 `昔 <{filename}/articles/2019/09/nftables-on-debian.rst>`_ 書いたので、そっちを参照してもらうのがいいだろう。僕が使ってる ruleset は https://github.com/mizunashi-mana/mizunashi-work-playbook/blob/04d0f8fc5701ef400d543bfa91bc45c4f056be4e/roles/nftables/files/etc/nftables.conf にある。良かったら、参考にどうぞ。今回は、ログ出力に関わる部分のみにとどめた nftables の使い方だけを見ていくことにする。

nftables には、ログ出力用の log statement があり、matcher / control flow と合わせて使うことができる。例えば、TCP 宛先ポート 80 のパケットを、1分に1エントリーのレートでログ出力するには、次のような文を入れる::

    tcp dport 80 limit rate 1/minute log prefix "[NFTABLES HTTP PKT]: " continue;

input filter に仕込むと該当のパケットが来た場合、カーネルログに次のようなメッセージが出る::

    May 21 08:01:58 internal kernel: [55242.749774] [NFTABLES HTTP PKT]: IN=lo OUT= MAC=00:00:00:00:00:00:00:00:00:00:00:00:86:dd SRC=fde4:8dba:82e1:1006:0000:0000:0000:1001 DST=fde4:8dba:82e1:1006:0000:0000:0000:1001 LEN=401 TC=0 HOPLIMIT=64 FLOWLBL=815899 PROTO=TCP SPT=59840 DPT=80 WINDOW=512 RES=0x00 ACK PSH URGP=0

基本的にカーネルログへ出る形式は、 ``フィールド名[=値]`` という感じ。prefix でどのパケットのログかを見分けることになる。flood 対策のログとかは全部流してしまうと、DoS 対策のフィルターのログでディスクが詰まって DoS が成立してしまうということになりかねないので、基本 rate limiter とセットがいいだろう。今回は紹介しないが、counter 機能を使えば、数は正確に計測ができる。ログ出力と付き合わせれば、ある程度監視が効くようになるだろう。

ulogd で出力先を変更する
-------------------------------

さて、監視には分析環境の整備とデータ整理が必要になる。特にデータ処理において、独自の形式のログだと色々取り扱いにくい。パケット監視にしても、やっぱりプロトコルの種類、送信元アドレス、宛先ポートなど、監視したい項目は色々あるだろう。そこら辺に関して、クエリが書けるようにはしときたい。そのような需要に対応するため、nftables のログは出力先の変更やフォーマットの変更に対応している。その中でも手軽な方法は、 `ulogd <https://www.netfilter.org/projects/ulogd/>`_ を使う方法だ。

まずは、 `ulogd`_ をインストールする。Debian では標準のパッケージとして提供されていて、 ``apt`` で入る::

    apt install ulogd2

後、JSON 形式での出力をサポートするため、JSON モジュールもインストールしておく::

    apt install ulogd2-json

``ulogd`` の設定ファイルは ``/etc/ulogd.conf`` にある。色々プラグインが用意されており、syslog や Graphite への出力などもサポートしているようだ。プラグインの種類は入力、フィルター、出力の3種類がある。今回は、

* 入力
    - NFLOG: nftables での基本的なパケットログ入力元。ま、基本入力はこれで良い。
* フィルター
    - BASE: 基本的なパケット情報をログに含める。
    - IFINDEX: ネットワークインターフェースをインデックスではなく名前でログる。
    - HWHDR: MACアドレスを文字列形式でログる。
    - IP2STR: IPアドレスを文字列形式でログる。
* 出力
    - JSON: JSON 形式でファイルに出力する。

を使っていく。他のプラグインについては、https://git.netfilter.org/ulogd2/tree/doc/ulogd.sgml を参照すると良いだろう。まず、使うプラグインを有効化しておく。 ``/etc/ulogd.conf`` の上部にある、plugin の定義で該当する箇所をコメントアウトしておく:

.. code-block:: ini

    plugin="/usr/lib/x86_64-linux-gnu/ulogd/ulogd_inppkt_NFLOG.so"
    #plugin="/usr/lib/x86_64-linux-gnu/ulogd/ulogd_inppkt_ULOG.so"
    #plugin="/usr/lib/x86_64-linux-gnu/ulogd/ulogd_inppkt_UNIXSOCK.so"
    #plugin="/usr/lib/x86_64-linux-gnu/ulogd/ulogd_inpflow_NFCT.so"
    plugin="/usr/lib/x86_64-linux-gnu/ulogd/ulogd_filter_IFINDEX.so"
    plugin="/usr/lib/x86_64-linux-gnu/ulogd/ulogd_filter_IP2STR.so"
    #plugin="/usr/lib/x86_64-linux-gnu/ulogd/ulogd_filter_IP2BIN.so"
    #plugin="/usr/lib/x86_64-linux-gnu/ulogd/ulogd_filter_IP2HBIN.so"
    plugin="/usr/lib/x86_64-linux-gnu/ulogd/ulogd_filter_PRINTPKT.so"
    plugin="/usr/lib/x86_64-linux-gnu/ulogd/ulogd_filter_HWHDR.so"
    #plugin="/usr/lib/x86_64-linux-gnu/ulogd/ulogd_filter_PRINTFLOW.so"
    #plugin="/usr/lib/x86_64-linux-gnu/ulogd/ulogd_filter_MARK.so"
    plugin="/usr/lib/x86_64-linux-gnu/ulogd/ulogd_output_LOGEMU.so"
    #plugin="/usr/lib/x86_64-linux-gnu/ulogd/ulogd_output_SYSLOG.so"
    #plugin="/usr/lib/x86_64-linux-gnu/ulogd/ulogd_output_XML.so"
    #plugin="/usr/lib/x86_64-linux-gnu/ulogd/ulogd_output_SQLITE3.so"
    #plugin="/usr/lib/x86_64-linux-gnu/ulogd/ulogd_output_GPRINT.so"
    #plugin="/usr/lib/x86_64-linux-gnu/ulogd/ulogd_output_NACCT.so"
    #plugin="/usr/lib/x86_64-linux-gnu/ulogd/ulogd_output_PCAP.so"
    #plugin="/usr/lib/x86_64-linux-gnu/ulogd/ulogd_output_PGSQL.so"
    #plugin="/usr/lib/x86_64-linux-gnu/ulogd/ulogd_output_MYSQL.so"
    #plugin="/usr/lib/x86_64-linux-gnu/ulogd/ulogd_output_DBI.so"
    plugin="/usr/lib/x86_64-linux-gnu/ulogd/ulogd_raw2packet_BASE.so"
    #plugin="/usr/lib/x86_64-linux-gnu/ulogd/ulogd_inpflow_NFACCT.so"
    #plugin="/usr/lib/x86_64-linux-gnu/ulogd/ulogd_output_GRAPHITE.so"
    plugin="/usr/lib/x86_64-linux-gnu/ulogd/ulogd_output_JSON.so"

次に、入力先と出力先の設定をする:

.. code-block:: ini

    [log17]
    group=17

    [json17]
    sync=1
    file="/var/log/ulog/nftables.log"

``log17`` は入力元の設定、 ``json17`` は出力先の設定になる。入力側の ``group`` は nftables 側で設定するグループに合わせる。nftables 側では log statement に設定する::

    tcp dport 80 limit rate 1/minute log prefix "[NFTABLES HTTP PKT]" group 17 continue;

group はそれぞれの statement 毎に指定可能で、ulogd 側でも複数のグループをそれぞれ扱うことができる。JSON 出力の設定には、出力先のファイルパスの指定 (``file``) と同期出力するかの設定 (``sync``)、他にもいくつか設定ができる。同期出力するかは、ログ出力を同期的に行う設定で、その分パケット処理のパフォーマンスは劣化するが、ログの反映が早くなる。

これらのプラグイン設定をもとに、以下のようにログ出力のスタックを組むことができる:

.. code-block:: ini

    stack=log17:NFLOG,base1:BASE,ifi1:IFINDEX,ip2str1:IP2STR,mac2str1:HWHDR,json17:JSON

これにより、 ``/var/log/ulog/nftables.log`` に以下のようなログが載るようになる::

    {"timestamp": "2023-05-21T10:54:05.058650", "dvc": "Netfilter", "raw.pktlen": 80, "raw.pktcount": 1, "oob.prefix": "[NFTABLES HTTP PKT]", "oob.time.sec": 1684666445, "oob.time.usec": 58650, "oob.mark": 0, "oob.ifindex_in": 4, "oob.hook": 1, "raw.mac_len": 14, "oob.family": 10, "oob.protocol": 34525, "raw.label": 0, "raw.type": 1, "raw.mac.addrlen": 6, "ip.protocol": 6, "ip6.payloadlen": 40, "ip6.priority": 0, "ip6.flowlabel": 960112, "ip6.hoplimit": 64, "ip6.nexthdr": 6, "src_port": 52938, "dest_port": 80, "tcp.seq": 3860706922, "tcp.ackseq": 0, "tcp.window": 64800, "tcp.offset": 0, "tcp.reserved": 0, "tcp.urg": 0, "tcp.ack": 0, "tcp.psh": 0, "tcp.rst": 0, "tcp.syn": 1, "tcp.fin": 0, "tcp.res1": 0, "tcp.res2": 0, "tcp.csum": 50192, "oob.in": "eth2", "oob.out": "", "src_ip": "fde4:8dba:82e1:1006::1001", "dest_ip": "fde4:8dba:82e1:1006::1002", "mac.saddr.str": "08:00:27:5b:f8:27", "mac.daddr.str": "08:00:27:2d:64:2b", "mac.str": "08:00:27:2d:64:2b:08:00:27:5b:f8:27:86:dd"}

後は適当にログコレクタで JSON パースして、分析環境に上げておけば、パケットの監視を行うことができる。

まとめ
-------------

というわけで、今回は nftables のログを JSON で別ファイルに出力する方法だった。ulogd インストールして、単純な設定するだけでできるので、割と簡単。

僕のサーバではとりあえずフィルタのログと、後出力パケットを limit かけてログって fluent-bit でパースして ElasticSearch に上げてる。で、プロトコル種別、宛先ポート番号とか別での時間帯によるパケット数推移とかを見てる。出力パケットからマルウェア検知とかできることもあるし、まあログを解析しやすいようにしておくと何かと便利。ま、今時 nftables 直接触ってる人も少ないと思うが、何か参考になれば。では、今回はこれで。
