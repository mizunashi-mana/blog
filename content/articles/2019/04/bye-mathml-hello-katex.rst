MathML にバイバイして， KaTeX に移行した
========================================

:tags: Pelican, MathML, ブログ, reStructuredText, JavaScript, CSS
:category: 運用

以前ブログで数式環境を， MathML と MathJax で導入した．ただ色々敗北したので KaTeX を使うことにした．その備忘録．

MathML に敗北した
-----------------

正確には， MathML は悪くないんだが，現状の MathML + MathJax 環境は色々辛いものがあった．

* ``\mathit`` や ``\mathrel`` などの基本的なコマンドに対応していない．
* MathJax のレンダリングが遅い．
* MathJax がでかい．

みたいな感じで，もうちょっとなんとかしたかった．基本的なコマンドに対応していない件は， ``docutils`` のデフォルトトランスパイラを使っているのが悪いだろうと思って， ``latexml`` を試してみたんだが，こちらは ``\mathbb`` に対応していなかったり．そもそも，デフォルトの MathML の段階でフォントの制約がある以上，コマンドはかなり限られてくる．そこら辺のつらみが増した結果， MathML は諦めることにした． MathML を諦めるなら，特に MathJax にこだわる必要もないので， MathJax ともおさらばすることにした．

KaTeX の導入
------------

結局 ``docutils`` には生の LaTeX ソースを吐き出させて， KaTeX でレンダリングする感じに落ち着いた． ``docutils`` に LaTeX ソースそのままを吐き出させるには， ``pelicanconf.py`` で以下のように書けばいい::

  DOCUTILS_SETTINGS = {
    ...,
    'math_output': 'LaTeX',
  }

後は， KaTeX をインストールしてレンダリングスクリプトを書いていく::

  npm i -S katex

.. code-block:: javascript

  import KaTeX from 'katex';
  import 'katex/dist/katex.min.css';

  export function renderMathBlock() {
    for (const e of document.querySelectorAll("article.single pre.math")) {
      try {
        const ne = document.createElement('div');
        ne.setAttribute('class', 'math block');

        const mathText = e.textContent;
        e.parentNode.replaceChild(ne, e);
        KaTeX.render(mathText, ne);
      } catch (e) {
        console.error(e);
      }
    }

    for (const e of document.querySelectorAll("article.single tt.math")) {
      try {
        const ne = document.createElement('span');
        ne.setAttribute('class', 'math inline');

        const mathText = e.textContent;
        e.parentNode.replaceChild(ne, e);

        KaTeX.render(mathText, ne);
      } catch (e) {
        console.error(e);
      }
    }
  }

reStructuredText が対応している数式モードは，ディレクティブとロールの 2 種類あり，ディレクティブは ``pre.math`` ，ロールは ``tt.math`` の中身に LaTeX ソースを埋め込むので，それを KaTeX でレンダリングするだけ．後は， Webpack で CSS ロードとフォントロードの仕組みを導入してやるだけで良い．

PostCSS の導入
--------------

KaTeX の為に CSS ローダを追加するついでに， PostCSS を導入することにした．まず， KaTeX のための Alias を追加する::

  resolve: {
    alias: {
      ...,
      katex$: 'katex/dist/katex.mjs',
      katex: 'katex',
    }
  },

それから， CSS とファイルローダを追加する．CSS を JavaScript と分離するため， ``mini-css-extract-plugin`` を使っている::

  module: {
    rules: [
      {
        test: /\.(scss|css)$/,
        use: [
          MiniCssExtractPlugin.loader,
          {
            loader: 'css-loader',
          },
          {
            loader: 'postcss-loader',
            options: {
              plugins: function () {
                return [
                  require('cssnano')({
                    preset: ['default', {
                      mergeRules: false,
                    }]
                  }),
                  require('precss'),
                  require('autoprefixer')
                ];
              }
            }
          },
        ]
      },
      {
        test: /\.(ttf|woff2?)$/,
        use: [
          {
            loader: 'file-loader'
          },
        ]
      }
    ],
  },

  plugins: [
    new MiniCssExtractPlugin({
      filename: '[name].css'
    })
  ],

PostCSS の cssnano で mergeRules を無効にしているのは，マージアルゴリズムがバグって [#cssnano-mergerules-bug]_ ツールチップのために指定した CSS のプロパティが消えたため．後は，既存の CSS ファイルを PostCSS で書き直した．これで適当にベンダープレフィックスとかも入れてくれる．

それから，数式のスタイルも少しいじった::

  article.single {
    div.math.block {
      overflow-x: auto;
      overflow-y: hidden;
      text-align: center;
    }
  }

overflow 時の設定を加えて，センタリングされるようにした． ``overflow-y: hidden`` を加えてるのは，なぜか KaTeX の方で数式の下に変な余白が生まれ，謎のスクロールが発生していたため．後はエントリーポイントの JavaScript に CSS をインポートするよう記述を加えるだけ．

まとめ
------

これで JavaScript が必須のブログになってしまった (LaTeX の脳内レンダリングできる人なら読めるが) ．ま，今の時代 JavaScript 無効にしてる人なんておらんやろ．

PostCSS も導入できたし，大分ブログの管理状況は人権があるものになったんじゃないかな．MathML が使えなくなったのはちょっと残念だけど．

.. [#cssnano-mergerules-bug] https://github.com/cssnano/cssnano/issues/701
