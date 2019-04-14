MathML で数式を表示する
=======================

:date: 2019-04-14
:tags: Pelican, MathML, ブログ, reStructuredText, MathJax, JavaScript
:category: 運用

reStructuredText は数式表示に対応している．ただデフォルトのレンダリング表示がいまいちだったので，ちょっといじった．その備忘録．

math ディレクティブと docutils の設定
-------------------------------------

reStructuredText には math ディレクティブというものが標準で搭載されていて [#math-directive-spec]_ ， ``TeX`` の記法を使って以下のように数式が埋め込める::

  .. math::

    \forall a, b \in \mathbb{N}.\, a \leq b \implies \sqrt{a} \leq \sqrt{b}

これは特に何もしなくても ``docutils`` でコンパイルして HTML にしてくれる．なので， reStructuredText を使っている場合は簡単な数式なら Pelican のブログ記事で普通に使える．これ自体は素晴らしいんだが，出される HTML は数学記号を Unicode 表現にして変数をイタリックにしただけで，空白制御とかはあまり考慮されておらず，フォントもあまり良くない．なので，もうちょっと何とか出来ないかということで，色々調べた．

``docutils`` の出力オプションとして `math_output <http://docutils.sourceforge.net/docs/user/config.html#math-output>`_ というオプションがあり色々弄れるらしい．ここに指定できる出力の仕方は，以下の 4 通り:

HTML
  デフォルトの出力方法． ``i`` タグや ``sup`` タグなどを駆使して簡単な数式レンダリングを行う．カスタムCSSファイルを引数として指定すると， CSS が必要な時だけ埋め込んでくれる．

MathJax
  MathJax を使ってレンダリングを行う．引数に MathJax のライブラリファイルの指定が必須．

MathML
  MathML を使ってレンダリングを行う．引数にコマンドを指定すると，そのコマンドでコンパイルしてくれる．

LaTeX
  数式表現をそのまま埋め込む．

で，今回は気になったので MathML を使ってみた．引数にコマンドを指定しない場合標準のコンパイラで MathML に変換してくれる．なお，標準のは ``\ldotp`` が使えなかったりちょっと不便なところがあるが， ``latexml`` などを使うと使える記号をもうちょっと増やせるみたい．今回は，妥協してフルに ``LaTeX`` 使いたかったら PDF で埋め込めばええやろという気持ちで，標準のをそのまま使うことにした．

``pelicanconf.py`` に次の設定を足すだけで， MathML が使えるようになる:

.. code-block:: diff

    DOCUTILS_SETTINGS = {
      ...
  +   'math_output': 'MathML',
    }

MathML のフォールバック
-----------------------

ところで， MathML を使う場合気になるのが，ブラウザの対応状況だ．というわけで， `サポート状況 <https://caniuse.com/#feat=mathml>`_ を覗きに行ってみると，見事に壊滅状態にあってサポートしてるの Firefox / Safari ぐらいやんってなった． Edge は現状サポート予定はなくて， Chrome は提案は出てるもののやっぱりサポート予定はないっぽい．

なので， MathML のフォールバックを行うことを考えたい． MathJax が MathML での入力に対応してるので導入してみたのだが，これが webpack とかなり相性が悪い構成になっているっぽい．本家にも `イシュー <https://github.com/mathjax/MathJax/issues/1629>`_ が立っているわけだが，現状の構成でとても対応できるとは思えない．で，色々探っていたら，次期 MathJax v3 では TypeScript を使って書いていていい感じにモジュール構成を取り入れているため， webpack にももちろん対応できるという話が上がっていた．で，まだ正式リリースじゃなくてベータ版なんだけど，使ってみるかってなった．どうせレンダリング失敗しても， MathML 見れる環境なら問題ないはずだし．

まず， npm で ``mathjax3`` をインストールする::

  npm install -S mathjax3

で， ``webpack.config.js`` に以下の設定を足す:

.. code-block:: diff

    resolve: {
      alias: {
        ...
  +     mathjax3$: 'mathjax3/mathjax3/mathjax.js',
  +     mathjax3: 'mathjax3/mathjax3',
      }
    }
    ...
    plugins: [
      ...
  +   // to disable asyncLoad()
  +   new webpack.NormalModuleReplacementPlugin(
  +     /AsyncLoad\.js/,
  +     (resource) => {
  +       if (resource.context.endsWith('mathjax3/util')) {
  +         resource.request = resource.request.replace(/AsyncLoad/,"AsyncLoad-disabled");
  +       }
  +     }
  +   )
    ],

最初の設定はいいとして，2番目の設定についてだけど， webpack はコード解析して依存関係を解決してるんだけどその解析が MathJax v3 の ``AsyncLoad`` モジュールで死ぬという問題があるらしい．で， webpack 使ってバンドルする場合そもそも非同期 import いらないねって話があって， MathJax v3 では webpack 向けに ``AsyncLoad-disabled.js`` というモジュールを提供することにしたらしい [#mathjax3-issue-88]_ ．で， webpack を使う場合は ``AsyncLoad`` モジュールを ``AsyncLoad-disabled`` モジュールに置換することでその問題を解決できる．それをやるのが上のコード．

後は， JavaScript で，

.. code-block:: javascript

  import { MathJax } from 'mathjax3';
  import { MathML } from 'mathjax3/input/mathml';
  import { CHTML } from 'mathjax3/output/chtml';
  import { browserAdaptor } from 'mathjax3/adaptors/browserAdaptor';
  import { RegisterHTMLHandler } from 'mathjax3/handlers/html';

  RegisterHTMLHandler(browserAdaptor());

  const MathJaxDocument = MathJax.document(document, {
    InputJax: new MathML(),
    OutputJax: new CHTML({
      fontURL: 'https://cdn.rawgit.com/mathjax/mathjax-v3/3.0.0-beta.3/mathjax2/css',
    }),
  });

  export function loadMathJaxDocument() {
    return MathJaxDocument
      .findMath()
      .compile()
      .getMetrics()
      .typeset()
      .updateDocument()
      ;
  }

みたいなんを書いて， DOM のロード後のタイミングで ``loadMathJaxDocument`` 関数を呼び出せば良い． MathJax v3 は結構モジュールの導入のおかげで治安が良くなってて良い．以下が最終的なレンダリング結果になる:

.. math::

  \forall a, b \in \mathbb{N}.\, a \leq b \implies \sqrt{a} \leq \sqrt{b}

レンダリングがちゃんと出来ていなかったら，ぜひ使ってるブラウザを教えて欲しい．修正するかは分からないけど．

まとめ
------

ブログの数式表示を改善した． MathJax v3 がまだベータ版なので，ちょっとまめに更新は確認していきたい．

後ちょっとレンダリングが遅めなのが気になるがまあしゃーないね． MathML 標準対応してほちい．

.. [#math-directive-spec] http://docutils.sourceforge.net/docs/ref/rst/directives.html#math
.. [#mathjax3-issue-88] https://github.com/mathjax/mathjax-v3/issues/88
