ブログに共有ボタンを追加した
============================

:tags: Pelican, ブログ, Python, JavaScript
:category: 運用

ブログに共有ボタンを追加して， JavaScript を呼べるようにした．

導入までの経緯
--------------

Pelican には CSS を追加する仕組みはあるが， JavaScript を追加する仕組みはない．なので，テーマで対応していない JavaScript を使う機能は入れられない．これでは不便なので， JavaScript を無理やり追加することにした．

また，共有ボタンも現状テーマで対応していないため，無理やり JavaScript で追加することにした．静的に埋め込むこともできるのだが，どうせ JavaScript の機能が必要になる共有ボタンもあるので， JavaScript 有効下のみ共有ボタンが表示されることになっても問題ないであろう．しかも，共有ボタンの位置も下の方に設置することにしたので，読み込みの遅延をあまり感じることもないだろう．

で，結構色々大変だったので，記事にまとめておくことにした．

Pelican のプラグイン追加
------------------------

まず，ブログ独自のプラグインを作って，そのプラグインで JavaScript を埋め込むことにした．

プラグインの作り方は， https://docs.getpelican.com/en/stable/plugins.html#how-to-create-plugins を見れば概要は載ってるが，正直分からない．基本的には， ``signals`` をインポートして， ``register`` 関数でフックしたいイベントを登録する．イベントが呼ばれる位置は， Pelican のソースを見るしかない．そして見ても分からない．

しょうがないので， `プラグインのソース <https://docs.getpelican.com/en/stable/plugins.html#how-to-create-plugins>`_ を見ながらニュアンスでやっていくしかない．とりあえず， `better_tables プラグインのソース <https://github.com/getpelican/pelican-plugins/blob/master/better_tables/better_tables.py>`_ を見てみると， ``content_object_init`` をフックするとそれぞれのページ内容がレンダリングされた HTML を弄れることが分かる．主に，

.. code-block:: python

  def convert_content(content):
    content._content = some_process(content._content)

  def register():
    signals.content_object_init.connect(convert_content)

的なことを書くと，それぞれのページ内容に対して ``convert_content`` が呼ばれ， ``content._content`` にレンダリングされた内容が文字列で入っているためそれを弄ることができるようだ．気をつけて欲しいのは，レンダリングされた内容はあくまで reStructuredText などを HTML に変換した内容であってそれぞれのテーマの全 HTML がとれるわけではないことだ．

今回はこの ``content_object_init`` を使って適当に HTML を BeautifulSoup を使ってパースし，最後の方にページのメタ情報と JavaScript ファイルを埋め込んでいる．かなり雑だがソースは， https://github.com/mizunashi-mana/blog/tree/master/plugins/custom/embed_customjs から見れる． ``pelicanconf.py`` に ``JS_FILE`` で埋め込みたい JavaScript ファイルを指定するとそのファイルを埋め込むようになっている．あと，ページ内容ごとのメタデータを埋め込んでいる．

共有ボタン追加の JS コード
---------------------------

後は共有ボタン追加の JavaScript を書くだけだ．まず適当に webpack の設定を以下のように書いた

.. code-block:: javascript

  const webpack = require('webpack');
  const path = require('path');

  module.exports = {
    mode: 'production',

    entry: {
      bundle: './js/index.js',
    },

    output: {
      filename: '[name].js',
      path: path.resolve(__dirname, 'content/dist-asset')
    },

    resolve: {
      alias: {
        vue$: 'vue/dist/vue.esm.js',
      }
    },

    performance: {
      maxEntrypointSize: 400 * 1024,
      maxAssetSize: 400 * 1024,
    },
  };

バンドル後のスクリプトは vue を埋め込むと webpack 標準のサイズ上限を超えるので，適当に増やす．後，必要なものをインストールする::

  npm init # 質問にいい感じに答える
  npm install -D webpack webpack-cli
  npm install -S vue

後は， DOM の読み込みイベントにリスナー追加して，そこで共有ボタンの DOM を追加するコードを走らせる． DOM は Vue.js で生成して追加．コードは， https://github.com/mizunashi-mana/blog/blob/master/js/add_sharebuttons.js においてある．まあなんてことはない Vue のコードです，はい．

生成したコードは， Flex テーマの `Social の部分 <https://github.com/alexandrevicenzi/Flex/blob/v2.2.0/templates/base.html#L122>`_ を借りた．と言っても微調整は必要で，デザインをちょっちいじった．

まとめ
------

ドキュメントがあまりない分野をいじるのはしんどかった．まあしゃあないね．

JavaScript を任意に追加できるようになったので，数式環境を KaTeX でやるやつとかも近いうちにやりたい．ただ， KaTeX サポートは実はテーマの方でやってもらう方が良いのでは？ とも思っている．一度導入してみて，それを切り出して Flex テーマの方に PR 送ろうかな．

てことで，今回は以上．
