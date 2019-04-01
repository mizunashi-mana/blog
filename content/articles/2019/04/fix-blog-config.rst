ブログのデザイン調整
====================

:date: 2019-04-02
:tags: Python, Pelican, ブログ, CircleCI
:category: 運用

ブログの改良をちょっと色々やってたので，そのご報告．

Circle CI での謎の警告解消
------------------------------

今まで Circle CI 上だと以下の警告が出てた::

  WARNING: Locale could not be set. Check the LOCALE setting, ensuring it is valid and available on your system.

ビルド自体は問題ないんだけど，気になるので修正したいなあと思ってた．

この警告は Pelican が出してるもので，以下が該当部分:

.. code-block:: python

  # https://github.com/getpelican/pelican/blob/4.0.1/pelican/settings.py#L500

  # try to set the different locales, fallback on the default.
  locales = settings.get('LOCALE', DEFAULT_CONFIG['LOCALE'])

  for locale_ in locales:
      try:
          locale.setlocale(locale.LC_ALL, str(locale_))
          break  # break if it is successful
      except locale.Error:
          pass
  else:
      logger.warning(
          "Locale could not be set. Check the LOCALE setting, ensuring it "
          "is valid and available on your system.")

どうやら ``locale.setlocale`` が ``ja_JP`` を設定しようとするとエラーを出してるっぽい．

Circle CI の良いところは Docker イメージをそのまま使ってくれるのでエラーの特定が楽ってところ．
というわけで，実際にどういうエラーを吐いて警告が出るに至ってるのか， Docker で調べてみた::

  $ docker run -it circleci/python:3.7 bash
  circleci:/$ python -c 'import locale; locale.setlocale(locale.LC_ALL, "ja_JP")'
  Traceback (most recent call last):
    File "<string>", line 1, in <module>
    File "/usr/local/lib/python3.7/locale.py", line 604, in setlocale
      return _setlocale(category, locale)
  locale.Error: unsupported locale setting

てことで， ``locale.setlocale`` が失敗してるって時点で何となく予想はついてたけど， ``ja_JP`` のロケール情報がインストールされてないのが問題っぽい．そこまでは予想がついたんだけど，じゃあ具体的にどう対処すればいいんだろうってとこがちょっと分からなくて，色々調べた．

Circle CI は通常 Debian を使ってるっぽい．一応 Debian であることを明示する為， ``-stretch`` とかいう接尾辞が付けられるっぽいので付けといた [#circleci-python-images]_． 現在インストールされてるロケールは， ``locale`` コマンドで調べられるらしい::

  circleci:/$ locale -a
  C
  C.UTF-8
  POSIX

ここに ``ja_JP`` が表示されれば勝ち．でロケールをインストールするには，インストールしたいロケールを ``/etc/locale.gen`` で見つけてそこのコメントを外し， ``locale-gen`` コマンドを走らせるか， ``dpkg-reconfigure locales`` をすればいいらしい．なので，以下のコマンドを Circle CI の設定に追加した::

  sudo sed -i 's/# ja_JP\.UTF-8/ja_JP\.UTF-8/' /etc/locale.gen
  sudo update-locale LANG=C.UTF-8 LC_MESSAGES=POSIX
  DEBIAN_FRONTEND=noninteractive sudo -E dpkg-reconfigure locales

これで完璧やろw とか思ってプッシュしたがなんかエラーが治らない．試しに Docker 上でやってみると，以下のようになった::

  circleci:/$ locale -a
  C
  C.UTF-8
  POSIX
  ja_JP.utf8

は？ ``ja_JP.utf8`` だけしか追加されとらんやんけ．どういうこっちゃって感じ．でさらに調べると，どうやら，ロケールのエイリアス情報が ``/usr/share/locale/locale.alias`` って場所にあるらしく，ロケールのインストールの時はそこの名前も登録されるらしい．で，そこを見てみると，以下のようになってた::

  circleci:/$ cat /usr/share/locale/locale.alias | grep ja_JP
  japanese        ja_JP.eucJP
  japanese.euc    ja_JP.eucJP
  ja_JP           ja_JP.eucJP
  ja_JP.ujis      ja_JP.eucJP
  japanese.sjis   ja_JP.SJIS

あ，ふーん．今は2019年， UTF-8 標準の時代やぞ．というわけで閉廷した．と言いたいとこだが，閉廷しても警告は治らないので，以下のコマンドを追加することにした::

  sudo sed -i 's/ja_JP\s.*$/ja_JP ja_JP.utf8/g' /usr/share/locale/locale.alias

本来は Pelican の設定を変えるか， ``ja_JP.eucJP`` を使うべきなんだろうが，ロケール情報は ``gettext`` も使うと思われるのでマップ情報があるのは好ましくないだろうし，かといって eucJP を使うのは敗北感が強かったので間をとって（？）こうした．これにより，ちゃんとロケールが認識されるようになった::

  circleci:/$ locale -a
  C
  C.UTF-8
  POSIX
  ja_JP
  ja_JP.utf8

ついでに， cache のバージョニングもするようにして，壊れた cache が生成された時にバージョンを上げることで古いキャッシュを捨てられるようにした．

アンカーリンクの追加
--------------------

各節に飛ぶリンクを表示するようにした．これは， `headerid <https://github.com/getpelican/pelican-plugins/tree/master/headerid>`_ というプラグインがありこれを使った．単に ``pelicanconf.py`` に

::

  HEADERID_LINK_CHAR = '<i class="fas fa-link anchor-link"></i>'

を足して，ちょっとデザインとかをいじっただけ．

(ところで幾つかプラグインの実装を見てたんだけど，みんな HTML を Beautiful Soup でパースしてゴリ押ししてたりするんだが，大丈夫なんか... こういうもんなんだろうか...)

VSCode のプラグインの修正
---------------------------

`前の記事 <https://mizunashi-mana.github.io/blog/posts/2019/03/start-blog/#auto-id-10>`_ で言ってた， reStructuredText の VSCode プラグインのバグを修正した: https://github.com/vscode-restructuredtext/vscode-restructuredtext/pull/153 ．

内容は， ``docutils`` の ``column_width <https://sourceforge.net/p/docutils/code/HEAD/tree/tags/docutils-0.14/docutils/utils/__init__.py#l643>`` 関数に合わせて下線の長さを修正するだけなんだけど，思ったより難しかった．

最近は，文字コードの世界は Unicode という規格で統一されつつあるわけだけど，この規格が結構罠が多い (正確には文字の世界は複雑すぎるということなんだけど) ．で，よく槍玉に上がるのが

East Asian Width
  https://www.unicode.org/reports/tr11/ で規定されている，文字幅の参考特性．半角とか全角とか，半角相当・全角相当みたいな感じのクラス分けをしている． (ただこの特性値自体も罠がある [#eaw-problem]_)

Combining Character
  「ă」は2文字 U+0074 U+0306 で表現される．このうちの後ろの文字 U+0306 が Combining Character と呼ばれるもので，前後の文字にかかったり，異次元に飛んでったりして，単体ではあまり意味を持たないやつ．日本人的には濁点が一番親しみやすそう． Unicode のデータベースでは Combining class というパラメータが提供されていて，どういう Combining を行うかが分かるようになっている． 0 だと Combining Character ではない．それぞれの値の意味は， http://www.unicode.org/reports/tr44/#Canonical_Combining_Class_Values を参照．

Surrogate Pair
  UTF-16 という Unicode のエンコード方式が，世界の文字の固定サイズ表現を目指して大敗北した話は有名だけど，大敗北の余波を自分たちが存分に受けてることを知らない人は結構多い． JavaScript という言語は諸にこの余波を受けている． JavaScript の文字列の内部は 16bit 非負整数の配列だ．これだけだと他の言語も事情が同じものが多いわけだけど，問題は 文字列の長さ = ``String.prototype.length`` が返してくる数値 = 配列の長さ と規定してしまった点だ．実際には Unicode 規格に含まれる文字は 16bit などでは納まりきらず， 16bit 2つを組み合わせて1つの文字を表す表現を併用することとなった．この組が Surrogate Pair だ．もし文字列の長さを「Unicodeにおける文字単位でいくつ分か」として取得したいなら， ``'おすし🍣'.length`` としてはいけない．「🍣」は Surrogate Pair で表現される文字で，この結果は

  ::

    'おすし🍣'.length == 5

  となる．最近の JavaScript での Surrogate Pair を考慮する場合のイディオムは，

  ::

    [...'おすし🍣'].length == 4

  だ．文字列のイテレータは，Unicode 単位で文字を分解してくれるようになっているので，こちらだと Surrogate Pair に関する問題は起きない．

それぞれの特性規定自体は，そこまで大きな問題は孕んでいないと思う (詳しくないので知らない) けど，問題はこれらが日々アルファベット以外の文字を扱う人たちの間でしか発現しないこと，そして結果実装者が規格の存在を知っているかによって実装が大きく異なることであり，それによって実装の差異を吸収するための規格が実装によって大きな差異を生み出してしまうことにある．この辺の話は詳しい人に聞くと (外野から見てる分には) 中々面白いが，僕はあまり知識も経験もないのでここら辺でやめとく．ついでに今回は，この三つの役満だった．

``docutils`` の `実装 <https://sourceforge.net/p/docutils/code/HEAD/tree/tags/docutils-0.14/docutils/utils/__init__.py#l643>`_ は East Asian Width と Combining Character を考慮した実装になっていて， East Asian Width での文字の長さを換算した後， Combining Character の分を差し引いて，最終結果を出している．それに対して，プラグインの実装は単に ``length`` で長さを計っていた．

これを， `meaw <https://github.com/susisu/meaw>`_ というライブラリのお世話になって修正した． ``meaw.computeWidth`` という関数は ``for``-``of`` で文字列の文字それぞれの East Asian Width を合計してくれる． ``for``-``of`` は文字列のイテレータで要素を回すため， spread operator を使う場合と同じく Unicode 単位で文字を分解してくれる．

Combining Character に関しては，色々調べたんだけど，文字を与えると Unicode データベースを単に返してくれるライブラリが見つからなかったので断念した (この辺はライブラリの大きさとのトレードオフなので仕方ないとも思うがうーんって感じ． Python は標準サポートが入っていて強いなあって思った．) ．なんかいい感じの方法知ってたら教えてもらいたい．ただ，下線は長い分には問題ないので，そこまで影響はないはず．一応， ``normalize`` で1つの文字にまとめられる系はまとめるようにしておいたし．

もう既に修正は取り込まれてるので，時期バージョンで治るのかな？

プライバシーポリシーの変更
----------------------------

ちょこっと修正した．個人情報は匿名化されるって言ってたけど， Google Analytics とか GitHub Pages とかは中で何やってるか分からないので，そっちでは特定とかやってるかもしれない的なことを足した．

特定しても個人的には特にメリットがないので，うちはしないです．

シェアボタンの追加のための調査
------------------------------

シェアボタンを追加したくて色々調査してた．最終的には， JavaScript でシェアボタン追加すればよくねってなったんだけど，そもそも自前の JavaScript を呼ぶ機構がテーマになさそう．プラグインでがんばって挿入とかできないかなとか試したけどよく分からんかった．

なので，もうちょっと後になりそう．

今後について
--------------

ちょっと最近忙しいので，次の記事投稿まで期間開くかもだけど，落ち着いたら色々書くかも．こちらからは，以上です．

.. [#circleci-python-images] https://circleci.com/docs/2.0/circleci-images/#python
.. [#eaw-problem] https://github.com/hamano/locale-eaw
