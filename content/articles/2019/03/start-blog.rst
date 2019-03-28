ブログ始めました
====================

:date: 2019-03-29 19:00
:tags: Python, Pelican, ブログ, GitHub
:category: 運用

色々あって，技術的なことを書くブログを始めようと思いました (まる) ．
今までは， Advent Calendar の時期だけ Qiita に記事を書いてたりしたんですが，
ちゃんとしてないやつも書いていこうかなってことで．

ちゃんとした系の記事は，出来るだけ外部に出していこうと思いますが，
調査不足だったり，よく分からない系とか徒然なるままな感じのことだったりとか，あと主に自分のためにしたこととかを書いていこっかなって感じです．

で，早速ですが一応ブログ立てたんで，立てる際した事とかを書いていく．

サイトジェネレータの選定基準
--------------------------------

まず，ブログに使うサイトジェネレータどうしよっかなあと悩んでいた．サイトジェネレータ一覧人気順は， https://www.staticgen.com/ から見れるっぽい．だいたい人気の上位陣は，

* Jekyll
* Next
* Hugo

っぽい． Hugo は使ったことあって，割と使いやすくはあったんだけど，今回は見送った．理由は Markdown しか基本的に書けないから．
常々， Markdown は明示的に拡張記法を導入できないし，脚注とかが拡張だったりして機能的にたりない面があったりで，ちょっと辛いなあと思ってた．
なのでブログでは， Org-mode か reStructuredText を使いたいなと思ってた．

* Org-mode の場合 Emacs に HTML 出力させればいいので，大半のジェネレータでいける
* reStructuredText は変換に Python の docutils を使うか， pandoc を使うといける

で，色々探してた結果， Python 製の Pelican というジェネレータがあるらしく，標準で reStructuredText に対応してたしハイライトは Pygments を使って何とかできそうで結構 Python を使うと色々ライブラリとの相性が良さそうだったので，今回はそれで行くことにしてみた．

テーマも良さそうなのがあったし，設定も結構簡単だったので，とりあえず満足してる．

Pelican の設定
------------------

https://docs.getpelican.com/en/stable/quickstart.html を見ながら，適当に自分の必要な感じでカスタマイズしてやった．

Pipenv の設定
::::::::::::::::::::

まずは， Pipenv を使って依存ライブラリを入れてく::

  pip install pipenv
  pipenv install pelican

Pelican の設定
:::::::::::::::::::

::

  pipenv run pelican-quickstart

をやって，適当に質問に答えていく．今回は， GitHub Pages にデプロイする前提なのでそういう感じで答えていく．あと， Makefile の方が使いたかったのでそっちを使って，作られた ``tasks.py`` は消しといた．ただ作られたやつそのままだとちょっと不便なので， ``Makefile`` の以下の箇所は変えた．

.. code-block:: diff

  - PELICAN?=pelican
  - GHP_IMPORT?=ghp-import
  + PELICAN?=pipenv run pelican
  + GHP_IMPORT?=pipenv run ghp-import

  - GITHUB_PAGES_BRANCH=master
  + GITHUB_PAGES_BRANCH=gh-pages

GitHub へのデプロイのためには， ``ghp-import`` というパッケージが必要っぽいのでそれも入れた::

  pipenv install ghp-import

後は， https://docs.getpelican.com/en/stable/settings.html を見ながら， ``pelicanconf.py`` と ``publishconf.py`` を適当にいじった．最終的な結果は，右上の GitHub ボタンからソースに飛べるので適当に見てください．

2つだけ言っておくと，

* ``pelicanconf.py`` の内容は選ぶテーマによってかなり書く内容が異なる．後， Pelican の本体とテーマでパラメータ名が異なったりする．なので，パラメータが効かなかった場合，テーマのテンプレートを読みにいかないといけない場合がある．
* ``make html`` では ``pelicanconf.py`` が， ``make publish`` では ``publishconf.py`` が参照されてビルドされる．この時， ``MENUITEMS`` などに他ページへのリンクを書いたりすると，公開する URL によってはリンクの調整が必要になったりする．今回は ``/blog/`` 以下に公開するので，リンクを ``/somefile`` とか書くとずれる．見た感じ Pelican ではこの調整をしてくれないので，今回は ``publishconf.py`` で調整を明示的にやるコードを書いた．

後ディレクトリ構成は，基本的に ``content`` の下に

* ``articles``: 記事を書いて置く．
* ``pages``: 記事でない静的文書コンテンツを置く．
* ``asset``: 文書でない静的ファイルを置く．

みたいな構成にした (``asset`` 以外は標準の配置) ．後 ``asset`` は ``STATIC_PATHS`` に指定して Pelican にコピーしてもらう必要があるので，それもやった．

テーマの選別
::::::::::::::

だいぶ見にくいけど， http://www.pelicanthemes.com/ からテーマが色々見れるっぽい．とりあえず良さそうで開発も活発そうな `Flex <https://github.com/alexandrevicenzi/Flex>`_ を使うことにした．

Git submodule を使ってテーマをリポジトリに追加する::

  git submodule add https://github.com/alexandrevicenzi/Flex.git themes/Flex

後は， ``pelicanconf.py`` に以下を足せばいい::

  THEME = './themes/Flex'

https://github.com/alexandrevicenzi/Flex/tree/master/docs を参考に ``pelicanconf.py`` を色々修正．後，気になったデザインを修正して， ``content/asset/custom.css`` に置いた．これを ``CUSTOM_CSS`` パラメータに指定すると Flex テーマが読み込んでくれる．

プラグインの設定
::::::::::::::::::

公式プラグインが全て1つのリポジトリで管理されてるっぽい．とりあえずそれを， Git submodule で追加::

  git submodule add https://github.com/getpelican/pelican-plugins.git plugins

後は， ``pelicanconf.py`` に以下を追加する::

  PLUGIN_PATHS = ['plugins']

とりあえず，

* ``tipue_search``
* ``related_posts``

の2つを導入しといた．まあ，機を見て他にも導入したり導入やめたりするかも．

プライバシーポリシーとか諸々の設置
:::::::::::::::::::::::::::::::::::::

左にある諸々を書いた． ``pages`` 以下に置くと自動で認識して表示される．順番を制御したかったので， ``pelicanconf.py`` に以下の記述を足した::

  PAGES_SORT_ATTRIBUTE = 'source_path'

このパラメータは Flex テーマで処理される．標準で ``PAGE_ORDER_BY`` というパラメータもあるんだけど，現状未対応っぽい [#support-page-order-in-flex-theme]_．後はページを表示したい順に，ファイル名に接頭辞で番号をつければ良い．

Google Analytics とかの導入を見据えて，いくつかのサイトを参考にしながらプライバシーポリシーを設置した．参考にしたのは以下のサイト:

Quora のプライバシー規定
  かなりの部分参考にしたって感じ．
  デザインは結構終わってるけど，かなり詳細に書いてあって好感が持てる内容だった．

  https://jp.quora.com/about/privacy

Google のプライバシーポリシー
  2番目に参考にした．
  こちらも内容が丁寧で好感が持てる．
  ただ， Quora とのユーザ層の違いと多分法務部がかなりちゃんとチェックしてるんだろうけど，
  ところどころぼかしてあったり，エンジニアの対外的な態度と違う内容 (危険性を説明すべきところを，まるで問題ないように説明している)
  が書いてあったりする．そこら辺は，さすが大企業だなって感じだった (こなみ) ．

  https://policies.google.com/privacy?hl=ja

Qiita のプライバシーポリシー
  一応参考にした．
  ただ，どっちかというと他の2つより規定みたいな感じだった．
  よく言えばちゃんとしている，悪く言えば説明する気がないみたいな感じ．
  正直どこら辺参照したかすら覚えてない．

  https://qiita.com/privacy

Google Analytics 的には， Cookie の使用と Google Analytics の使用が明示されていれば良いっぽい [#google-analytics-privacy-rule]_． 後は，免責事項とか適当に足したサイトの説明とかも足した．

GitHub Pages の設定
-----------------------

GitHub Pages の有効化
:::::::::::::::::::::::::

リポジトリの Settings から ``gh-pages`` ブランチの内容を GitHub Pages で表示させるようにした．

後，パーソナルサイトは今の所作る予定がないので， https://mizunashi-mana.github.io/ は https://mizunashi-mana.github.io/blog/ にリダイレクトされるようにした．内容は https://github.com/mizunashi-mana/mizunashi-mana.github.io/blob/ed6aebc132267360a7ccd95717d7b5fe64ec7fa2/index.html にあるけど，単純に ``refresh`` でリダイレクト指定して，それでだめなら JavaScript にフォールバックする感じ．

これで， ``make github`` すると GitHub Pages にデプロイされるようになる．

Circle CI による自動デプロイ環境
:::::::::::::::::::::::::::::::::::

事故を防ぐため， PR で記事を追加していくことにした． ``master`` はプロテクトして基本プッシュせず， PR がマージされた時に Circle CI を回すようにした．

Circle CI は https://circleci.com/docs/2.0/language-python/ を参考に Python のバージョンを 3.7 にして，色々書くみたいなことやった．最終的な内容は， https://github.com/mizunashi-mana/blog/blob/master/.circleci/config.yml から見れるので気になったらどうぞ．

後， ``gh-pages`` の方はビルドを止めるため，何のタスクもない設定ファイルを追加した::

  version: 2
  jobs: {}
  workflows:
    version: 2

後は， ``GITHUB_TOKEN`` 環境変数を既に Circle CI 使ってるプロジェクトから import してくればおk．一から作る場合は， https://github.com/settings/tokens から適当に発行すれば良さそう．

他にやりたいこと
------------------

現状幾つか解決したい問題と追加したい機能もあって，

* VSCode の reStructuredText プラグインのアンダーライン補完が日本語だとうまく動かないので，これを修正したい．
* 全文検索に対応したい．
* 各メニューを日本語対応させたい．
* シェアボタンを追加したい．
* Google Analytics を追加したい．

ぐらいはやりたい．

VSCode の件は，プラグインは日本語文字の width を 1 とカウントしているのに対し，
docutils の方は East-Asian Width を考慮して換算してるっぽくて [#docutils-column-width]_ ，
結果補完されるアンダーラインが短い判定を受けてしまうという感じです．
これぐらいならすぐ直せそうなので，近いうちに PR を送ると思います．

全文検索は， Tipue Search 用の JSON を吐き出してくれるプラグインが既にあるっぽくて [#pelican-tipue-search-plugin]_ ，
後はテーマが対応していれば問題ないみたい．
しかも，使ってるテーマのイシューに検索対応したいみたいなことが `書いてある <https://github.com/alexandrevicenzi/Flex/issues/49>`_ ．
なのでこちらも近いうちに， PR 送ってみようかなと．

後メニュー日本語対応は， https://github.com/alexandrevicenzi/Flex/blob/v2.2.0/translations/en/LC_MESSAGES/messages.po の翻訳版を投げれば良さそうなので，これも近いうちにやる．アーカイブページの日付表示も変えられるようにしたい．これも近いうちにやる．

シェアボタンを表示する仕組みはテーマにはなさそう． Disqus を追加するとついでに付いてくるやつで凌ごうか悩み中．

Google Analytics はアカウント設定がちょっと億劫でやってないけど，そのうちやる．

(やるとは言っていない)

あまり乗り気じゃないもの
-----------------------------

以下は一応考えてはいるけど，現状あんまり乗り気じゃないもの．

* 広告表示
* コメント機能
* CSS Rhythmic Sizing
* GDPR (ePrivacy) 対応

全部テーマで対応してるので，それほど追加は手間ではないんだけど，とりあえず様子見．

広告表示は，収益出るようだったら Google Adsense とか設置したいけど，今はいいかな．

コメント機能も Disqus を登録すればいいんだけど， Disqus 利用者そんなにいなさそうだしはてなブックマークとかで良さそう．

CSSについては，今は適当に ``line-height: 2.0;`` とかしてるんだけど， Vertical Rhythm を導入したい気持ちがある．ただ確か現状 CSS 標準ではなかったはずなので，ちょっと見送ってる (あんまり調べてない) ．気が変わるか安定したら対応するかもしれない．

GDPR (ePrivacy) 対応は現状は特に Cookie を使用していないので問題ないはずで，今後 Google Analytics や Google Adsense を導入する場合どうなるかの話だけど，正直どういう対応をすればいいか分からないので保留中．まあ， Cookie の制限については， ポップアップで Cookie の同意をとってから Google Analytics のコードとかを起動すればいいだけだし，その他については Google Analytics は修正条項が適用されたモードで起動すれば大丈夫っぽい．ただそもそも日本語で書かれたマイナーコンテンツを EU 圏がそこまで見るかって話だし熱心に対応する予定はない．

まとめ
---------

とりあえず，雑に続けていければなと思うので，よろしくお願いします．

.. [#docutils-column-width] https://github.com/docutils-mirror/docutils/blob/e88c5fb08d5cdfa8b4ac1020dd6f7177778d5990/docutils/utils/__init__.py#L628to
.. [#pelican-tipue-search-plugin] https://github.com/getpelican/pelican-plugins/tree/master/tipue_search
.. [#google-analytics-privacy-rule] Google Analytics の `利用規約 <https://www.google.com/intl/ja/analytics/terms/jp.html>`_ 7. プライバシー に記載あり．
.. [#support-page-order-in-flex-theme] https://github.com/alexandrevicenzi/Flex/issues/170
