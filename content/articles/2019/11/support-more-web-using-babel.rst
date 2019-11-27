babel と core-js を使って，古いブラウザをサポートする
=====================================================

:date: 2019-11-27 18:00
:tags: Web, JavaScript, Webpack, Babel, ブログ
:category: 運用

Sentry でたまに，古い OS からのアクセスによるエラーレポートが来る．現状，このブログの JS は ``let`` / ``const`` 普通に使ってるし， ``for-of`` なども使っている．なので，一世代前のブラウザなどを使っていると，そこら辺の機能に対応していなくてエラーになる．試しにこの辺を改善してみると，どうなるかのか観測してみたいなと思って，ちょっとサイト設定をいじってみた．そのメモ．

なお，使った環境は以下のもの:

+---------+---------+
| babel   | 7.7.4   |
+---------+---------+
| core-js | 3.4.2   |
+---------+---------+
| webpack | 4.41.2  |
+---------+---------+

Babel と PolyFill
-----------------

Sentry でたまに，以下のようなエラーが報告されていた．

* ``SyntaxError: Unexpected end of input``
* ``TypeError: document.querySelectorAll(...)[Symbol.iterator] is not a function``

おそらくこれらは，ブログの JavaScript が割と新しい構文を使っているためと思われる．で， Web の移行の黎明期は，こういった状況に対応するため，新しい構文を使用したプログラムからそれらを除いたプログラムへのトランスパイラと，古いオブジェクトに新しい API を追加する polyfill が活躍していた．最近は，セキュリティ的な面とこうした状況に対応していくため，ブラウザの自動更新機能が増強され， OS においてもかなりのサポートが入っており，これらを採用しなくても個人レベルなら何とかなる場合が多い．というか，うちのブログは今まで何とかなっていた．

トランスパイラや polyfill は，そういった背景もあって無数にあるのだが，その中で首位を占めているのが babel だ． babel は主にトランスパイラを提供しているが，その周辺ツールもまとめて色々提供しており， ``@babel/polyfill`` もその一つだ．ただ，現在は ``@babel/polyfill`` は非推奨になっていて，中身は ``core-js`` と ``regenerator-runtime`` というパッケージを読むようになっており，直接これらのパッケージを使うことが推奨されている． ``core-js`` は様々な polyfill をまとめてモジュール単位で提供しており， ``regenerator-runtime`` は generator や async/await などの機能をサポートするための generator のエミュレータが入った軽量のパッケージになっている． babel で吐き出したコードを動かすための ``@babel/runtime`` パッケージは，実際には幾つかの単純なヘルパ関数と ``regenerator-runtime`` を再エクスポートして提供しているだけだ．

実際に babel が書いた JavaScript をどうトランスパイルするのか見てみる．まず， babel の cli をインストールする::

  npm i -D @babel/core @babel/cli @babel/preset-env

``@babel/core`` が babel トランスパイラの本体． ``@babel/cli`` が ``@babel/core`` を使う CLI を提供してくれるパッケージになっている．あと， babel は吐き出す JavaScript でどんくらいのレベルの構文を使うか指定できて，デフォルトのいい感じにしてくれる出力を行う際使うパッケージが ``@babel/preset-env`` になっている．こいつらをインストールしたら，以下のような感じで試すことができる::

  $ echo "function* gen() { yield 1; }; export const g = gen();" \
    | npx babel --no-babelrc --presets @babel/preset-env
  "use strict";

  Object.defineProperty(exports, "__esModule", {
    value: true
  });
  exports.g = void 0;

  var _marked =
  /*#__PURE__*/
  regeneratorRuntime.mark(gen);

  function gen() {
    return regeneratorRuntime.wrap(function gen$(_context) {
      while (1) {
        switch (_context.prev = _context.next) {
          case 0:
            _context.next = 2;
            return 1;

          case 2:
          case "end":
            return _context.stop();
        }
      }
    }, _marked);
  }

  ;
  var g = gen();
  exports.g = g;

このコードは， ``@babel/runtime`` を別途読み込むことを前提としているが， ``@babel/plugin-transform-runtime`` プラグインを使うことで，明示的にモジュール読み込みをさせることもできる::

  $ npm i -D @babel/plugin-transform-runtime
  $ echo "function* gen() { yield 1; }; export const g = gen();" \
    | npx babel --no-babelrc --presets @babel/preset-env --plugins @babel/plugin-transform-runtime
  "use strict";

  var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

  Object.defineProperty(exports, "__esModule", {
    value: true
  });
  exports.g = void 0;

  var _regenerator = _interopRequireDefault(require("@babel/runtime/regenerator"));

  var _marked =
  /*#__PURE__*/
  _regenerator["default"].mark(gen);

  function gen() {
    return _regenerator["default"].wrap(function gen$(_context) {
      ...
    }, _marked);
  }

  ...

こんな具合に generator や async/await は ``regenerator-runtime`` を使ったコードに変換されるし， let / const や arrow function はただの var や function に変換される．ただ，これだけでは旧世代のブラウザに対応できない可能性がある．例えば， ``document.querySelectorAll`` が返す ``NodeList`` という抽象インターフェースは，通常の ``Array`` インスタンスと同様に扱えるように ``forEach`` メソッドやイテレータが搭載されているが，これが搭載されたのは Chrome でバージョン 51 から，　IE では搭載されていないというサポート状況になっている．そのため，これらのブラウザの上でサポートされている JavaScript の構文のみを使った場合でも，プログラム自体が意図通りに動かない可能性がある．実際， Sentry で報告されてきた ``TypeError`` はそれが原因だと思われる．

これらの API レベルのフォールバックを提供する polyfill で， babel で推奨されているパッケージが ``core-js`` だ．例えば， ``NodeList`` の ``forEach`` メソッドの polyfill は， ``core-js`` パッケージの ``core-js/modules/web.dom-collections.for-each.js`` というモジュールで提供されている．なお，ソースコードが大きくなることを許容するなら，すべての polyfill をまとめたモジュールとして ``core-js`` モジュールを単にインポートするだけでよい::

  import "core-js";

こうすることで，古い世代のクライアントでもある程度は動作が保証されることになる．

babel-loader を使う
-------------------

さて，このブログでは現状 Webpack を使ってモジュールシステムを管理している． Webpack 上で babel を使用したいなら， ``babel-loader`` パッケージを使えばよい．具体的には，まず以下のパッケージを依存に追加する::

  npm i -S @babel/runtime
  npm i -D @babel/core babel-loader @babel/preset-env @babel/plugin-transform-runtime

後は，以下の設定を ``webpack.config.js`` に追加する:

.. code-block:: diff

     module: {
       rules: [
         {
  +        test: /\.m?js$/,
  +        exclude: /(node_modules|bower_components)/,
  +        use: {
  +          loader: 'babel-loader',
  +          options: {
  +            presets: ['@babel/preset-env'],
  +            plugins: ['@babel/plugin-transform-runtime']
  +          }
  +        }
  +      },

``.mjs`` は Node.js などでサポートされている拡張子で， ES5 のモジュールシステムを使うプログラムに慣例として用いられているもの．うちではその拡張子を使ってないけど，とりあえず慣例として追加しておく．このようにしておけば，新しい構文は babel がいい感じにトランスパイルしてくれ，モジュールシステムは webpack がいい感じに解決してパッキングしてくれるようになる．後は， entrypoint のプログラムで ``core-js`` を読み込み用にしておけばよいだろう．

まとめ
------

とりあえず， Sentry のノイズ消しとお試しのためフォールバックするようにしてみた．これで，どんな感じになるかしばらく様子見てみようと思う．

うちのブログは JavaScript 切っても見れるよう，致命的なところに JavaScript 使っていないはずなので，特にアクセス状況に影響があるわけじゃないと思う．それに最近だとほとんどのブラウザは自動的に最新に保たれる (と信じてる) だろうし， JavaScript が動かない環境はほんの一握りな気がする．ただ，ブログを自分でコード管理しながら運営すると，こういう感じで色々アクセスに対して実験できていいなという感じなので，そういう実験はいろいろしていきたい．そういう感じで．
