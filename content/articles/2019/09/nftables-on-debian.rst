iptables から nftables への移行
===============================

:date: 2019-09-21 08:00
:tags: Debian, iptables, nftables, ファイアウォール
:category: 運用

Debian GNU/Linux 10 ，コードネーム buster が安定板リリースを迎え結構経ったので，そろそろアップグレード案件やるかみたいな感じになった．

さて， buster からは iptables に代わり nftables が採用されている．なので， iptables から nftables に移行が推奨されている．で，既存の iptables のルールセットを nftables 用に書き直したので，その備忘録．

なお，参考文献は以下だが，こちとらインフラは素人の普段はプログラマ屋さんなので結構間違ってるかもしれない．

* https://wiki.nftables.org/wiki-nftables/index.php/Quick_reference-nftables_in_10_minutes
* https://wiki.archlinux.jp/index.php/Nftables

nftables の基本
---------------

nftables はそこまで iptables と基本は変わらなくて，文法とかが異なったり永続化の方法がデフォルトで付いてたりとユーザ操作部分が分かりやすくなったっぽい．基本的には，

* ``nft`` コマンドで操作
* ``/etc/nftables.conf`` で永続化

みたいな感じ． ``nft`` は ``sbin`` にあるので，ルート権限で参照する必要がある．後，一応 Debian 10 では ``iptables`` の互換インターフェースと ``iptables`` からの移行用として ``iptables-translate`` というコマンドが用意されている．

なお， ``/etc/nftables.conf`` の読み込みは systemd のユニットファイルで指定されていて，再読み込みは， ::

  systemctl restart nftables

でできる．設定ファイルのチェックは， ::

  nft -c -f /etc/nftables.conf

でできる．現在の設定は， ::

  nft list tables
  nft list table inet filter

とかで確認できる． ``inet filter`` テーブルは， ``iptables`` の ``filter`` と対応する． ``iptables-translate`` は ``iptables`` のルールを ``nftables`` 用に変換してくれるコマンドで， ::

  $ iptables-translate -A INPUT -m state --state NEW -j DROP
  nft add rule ip filter INPUT ct state new  counter drop

みたいな感じで使える．なお，一括で変換する ``iptables-restore-translate`` も用意されてるっぽい． ``-f`` オプションでファイル指定もできる． ``iptables`` からの移行の際は，

1. 今のルールを ``iptables-save`` で吐き出させて
2. ``iptables-restore-translate`` に噛ませて変換し，その変換したルールを実行し
3. 最後に ``nft list ruleset`` の内容を ``/etc/nftables.conf`` に保存する

を実行すれば良い．まあちょっと調整は必要かもしれないけど．

nftables のルール
-----------------

さて， ``nftables`` では ``iptables`` のルールから文法が大きく変わった．ただ要素自体はそれほど変化はなさそう．ルールは階層構造で管理されていて，

1. table family (``inet`` / ``bridge`` / etc.)
2. table type (``filter`` / ``nat`` / etc.)
3. chain (``input`` / ``forward`` / ``output`` / etc.)

の順で指定できる．それぞれ多分好きに増やせるはず． ``/etc/nftables.conf`` だと以下のように階層構造で指定できる::

  table inet filter {
    chain input {
      type filter hook input priority 0; policy drop;
      ...
    }

    chain forward {
      type filter hook forward priority 0; policy drop;
      ...
    }

    chain output {
      type filter hook output priority 0; policy accept;
      ...
    }
  }

chain は上記のようにそれぞれフックを設定することもできる．後， ``policy`` でデフォルトのコントロールフローを設定できる．コントロールフローは以下の種類がある [#queue-operation]_:

accept
  パケットを通し，その場でルール評価を終了する

drop
  パケットを落とし，その場でルール評価を終了する

continue
  ルール評価を続ける

return
  現在の chain から呼び出し元の chain に戻って続きを評価する．なお，コールスタックに何もない場合 accept と同じになる

jump <chain>
  コールスタックに現在の chain の場所を追加し，指定した chain の評価を始める

goto <chain>
  コールスタックに何も追加しないで，指定した chain の評価を始める

で，それぞれ文の最後に指定できる． ``type`` と ``policy`` は特別な文で，他の文は次のような形をとる::

  <match>* <statement>* <controlflow>

``<match>`` は文でハンドルするパケットの種類を指定し， ``<statement>`` はパケットに対する操作を指定できる． ``<match>`` は，

meta
  パケットのメタデータに対するマッチ

ip
  IP パケットに対するマッチ

ip6
  IPv6 パケットに対するマッチ

icmp
  ICMP パケットに対するマッチ

icmpv6
  ICMPv6 パケットに対するマッチ

tcp
  TCP パケットに対するマッチ

udp
  UDP パケットに対するマッチ

ct
  conntrack エントリに対するマッチ

などのマッチが指定できる．マッチそれぞれにパラメータがあり，より詳細にハンドル文が書ける．例えば， ``ip`` マッチは ``saddr`` / ``protocol`` などのパラメータがあり，以下のような指定が書ける::

  ip protocol { tcp, udp } ip saddr != 192.168.2.0/24

パラメータに対するマッチは，幾つかの関係演算子が使えるようになっていて， ``==`` / ``!=`` / ``<`` などが使えるっぽい．なお，何も指定しないで空白区切で並べると，内部で implicit 演算子扱いになり，それぞれのパラメータでデフォルトの演算子 (大抵は ``==`` と同値) が指定されるっぽい．なので，上のは次のと同じ::

  ip protocol == { tcp, udp } ip saddr != 192.168.2.0/24

どのマッチでどのパラメータが使えるのかは， `nftables の man <https://www.netfilter.org/projects/nftables/manpage.html>`_ か `Quick reference <https://wiki.nftables.org/wiki-nftables/index.php/Quick_reference-nftables_in_10_minutes#Matches>`__ を参照すれば良い．

``<statement>`` の方は，

log
  ログ出力を行う

reject
  レスポンスパケットを指定して，パケットを落とす

counter
  パケットの数のカウンタを設定する

limit
  パケットのレート制限を設定する

みたいなものが設定できる．それぞれ指定できるパラメータは， `nftables の man <https://www.netfilter.org/projects/nftables/manpage.html>`_ か `Quick reference <https://wiki.nftables.org/wiki-nftables/index.php/Quick_reference-nftables_in_10_minutes#Statements>`__ を参照．まあ， ``iptables`` と大体同じことができる．

後， ``hashlimit`` の代替として ``meter`` という機能があるらしいが，現在は代わりに dynamic set / map と言うことになってるらしく，この辺もよく分かってない．時間あるときに調べないと．

設定例
------

とりあえず，今回作った簡単なファイアウォール設定を置いとく::

  #!/usr/sbin/nft -f

  flush ruleset

  table inet filter {
    chain input {
      type filter hook input priority 0;

      # default drop
      policy drop;

      # established/related connections
      ct state { established, related } accept;

      # loopback interface
      iif lo accept;

      jump filter_broadcast;
      jump filter_fragments;
      jump filter_invalids;
      jump filter_icmp;
      jump filter_synflood;
      jump reject_auth_ident;

      # accept TCP ports
      # SSH
      tcp dport { ssh } ct state new accept;

      # Web
      #tcp dport { http, https } ct state new accept;

      # Mail
      #tcp dport { smtp } ct state new accept;

      jump drop_others;
    }

    chain forward {
      type filter hook forward priority 0;

      # default drop
      policy drop;

      # established/related connections
      ct state { established, related } accept;

      # loopback interface
      iif lo accept;
    }

    chain output {
      type filter hook output priority 0;

      # default accept
      policy accept;
    }

    chain filter_broadcast {
      pkttype != { broadcast, multicast } return;
      drop;
    }

    chain filter_fragments {
      ip frag-off & 0x1fff 0 return;
      limit rate 6/minute burst 10 packets log prefix "[IPTABLES FRAGMENT]: " level debug continue;
      drop;
    }

    chain filter_invalids {
      ct state != invalid return;
      limit rate 6/minute burst 10 packets log prefix "[IPTABLES INVALID]: " level debug continue;
      drop;
    }

    chain filter_icmp {
      icmpv6 type {
        destination-unreachable,
        packet-too-big,
        time-exceeded,
        parameter-problem,
        nd-router-advert,
        nd-neighbor-solicit,
        nd-neighbor-advert,
      } accept;
      icmp type {
        destination-unreachable,
        router-advertisement,
        time-exceeded,
        parameter-problem,
      } accept;

      # echo-request
      jump filter_pingdeath;
      icmp type echo-request accept;
    }

    chain filter_pingdeath {
      icmp type != echo-request return;
      meter ping_scan { ip saddr limit rate 1/second burst 4 packets } return;
      limit rate 6/minute burst 10 packets log prefix "[IPTABLES PING_DEATH]: " level debug continue;
      drop;
    }

    chain filter_synflood {
      ct state != new return;
      meter syn_scan { ip saddr limit rate 2/second burst 100 packets } return;
      limit rate 6/minute burst 10 packets log prefix "[IPTABLES SYNFLOOD]: " level debug continue;
      drop;
    }

    chain drop_others {
      limit rate 6/minute burst 10 packets log prefix "[IPTABLES SCANED]: " level debug continue;
      drop;
    }

    chain reject_auth_ident {
      tcp dport { 113 } reject with tcp reset;
    }
  }

うちの環境は， IPv6 無効にしてるので IPv6 用には他にも色々書いといたほうがいいかもしれない．

まとめ
------

Debian buster にアップグレードした時の備忘録でした． ``iptables`` の諸々が改善されたのは良いが，結局よく分からん文法になってて，うーんと言う感じ．まあ，設定ファイルは前よりずっと見やすくなった感はある．ただ， ``nftables`` の文法，形式的なものが見つからなくてドキュメントもかなりゆるふわ感があり，結局本家の bison ファイルみに行く羽目になったのでそこら辺何とかして欲しいっすね．こちらからは以上です．

.. [#queue-operation] ``queue`` という操作もあるようだけど，あんまり深く調べてない．
