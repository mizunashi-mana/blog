脚注のためのツールチップ追加
============================

:tags: ブログ, Pelican, JavaScript, CSS
:category: 運用

脚注をツールチップで見れるようにしたので，それについて．

reStructuredText の脚注
-----------------------

reStructuredText では脚注が使える．これは，以下のように書けばいい::

  some text[#footnote-ref]_ .

  .. [#footnote-ref] some footnote content.

こうすると，自動でナンバリングが行われ，それぞれ脚注へのリンクと元の文書へのバックリンクが生成される．分かりづらいがバックリンクは，数字の部分に当てがわれ，それをクリックすることで戻れる．ただ，これをクリックするのがめんどかったので，ツールチップで表示してみることにした．

Tooltip.js を使う
-----------------

ツールチップライブラリとして， Popper.js が提供する Tooltip.js を使うことにした． Popper.js は Bootstrap がツールチップ表示のために使っているツールチップ専用の位置調整ライブラリ． Tooltip.js は Popper.js を使ってツールチップを表示してくれるライブラリだ．まずは， npm でインストールする::

  npm install -S tooltip.js

で，後は以下のようなことを書くだけ::

  new Tooltip(document.getElementById('some-id'), {
    placement: "bottom",
    title: "<p>some content</p>",
    html: true,
  });

デフォルトでは HTML は使えなくてサニタイズがちゃんとされる．今回は HTML の内容を表示するようにしている．こうすると， ``#some-id`` の要素にホバーすると，下にツールチップが表示されるようになる．今回は脚注の内容を登録したいので， JavaScript で脚注の内容を持ってきてツールチップを登録していく，以下の簡単なコードを書いた:

.. code-block:: javascript

  let footnoteTitles = {};
  for (const e of document.querySelectorAll("table.docutils.footnote")) {
    const eid = e.getAttribute("id");
    footnoteTitles[eid] = e.querySelector("td:not(.label)").innerHTML;
  }

  for (const e of document.querySelectorAll("a.footnote-reference")) {
    const eid = e.getAttribute("id");
    const reference = e.getAttribute("href").substr(1);

    const tooltipContent = document.createElement('div');
    tooltipContent.setAttribute('class', 'footnote-tooltip-content');
    tooltipContent.innerHTML = footnoteTitles[reference];

    new Tooltip(document.getElementById(eid), {
      placement: "bottom",
      title: tooltipContent,
      html: true,
    });
  }

ただ，ツールチップ導入の際気をつけたいことがあって，スマフォなどのタッチデバイスだとホバーイベントによる表示は逆にユーザ体験が悪いので，無効にしておくのが良い．ってことまでは思って，そういや Bootstrap のツールチップとかスマフォで表示されないよな，どうやってんだろと思ってソースを見に行った．そうすると，こんなことをしてた [#bootstrap-touch-device-detection]_:

.. code-block:: javascript

  // If this is a touch-enabled device we add extra
  // empty mouseover listeners to the body's immediate children;
  // only needed because of broken event delegation on iOS
  // https://www.quirksmode.org/blog/archives/2014/02/mouse_event_bub.html
  if ('ontouchstart' in document.documentElement) {
    $(document.body).children().on('mouseover', null, $.noop)
  }

``ontouchstart`` というリスナーがあればタッチデバイスだと認識してるっぽい．ついでにググると， https://stackoverflow.com/questions/4817029/whats-the-best-way-to-detect-a-touch-screen-device-using-javascript/4819886#4819886 みたいなんが出てくる．みんな苦労してるっぽい．今回はそこまで精度はいらなくて，最悪タッチデバイスじゃないとこでツールチップが表示されなくても問題ないなって感じなので， Bootstrap と StackOverflow の回答の合わせ技で，以下の判定でいくことにした::

  export function isTouchDevice() {
    return ('ontouchstart' in document.documentElement)
      || (window.DocumentTouch && document instanceof DocumentTouch)
      ;
  }

で， ``isTouchDevice`` な時はツールチップを登録しないようにした．

CSS を書く
----------

デフォルトでは， Tooltip.js は無地の ``z-index`` だけが調整されたツールチップを出してくる．なので，これにスタイル付けもしなきゃいけない．要素的には，

* ``.tooltip`` がツールチップ全体
* ``.tooltip .tooltip-arrow`` が矢印
* ``.tooltip .tooltip-inner`` が中身

になってるっぽい．まずツールチップに余白と背景，外形を設定しておく::

  .tooltip {
    background-color: #ffffff;
    border-radius: 6px;
  }

  .tooltip .footnote-tooltip-content {
    padding: 4px 10px;
  }

``footnote-tooltip-content`` は JavaScript でツールチップの中身を生成時に指定しておいたクラス．で，ここからが本体だが，まずツールチップのための三角形を作る．三角形を作る方法は割と知られている border を作る方法でやる．どういうのかというと， border の境目って斜めになってるじゃん？ これブロックの幅 0 にして余白空けて，余白の長さのボーダ作ったら三角形 4 つできるよね？ 3 つ透過にしたら 1 つ三角形手に入るじゃんとかいう，誰が考えたか分かんないけどその発想はなかったな奴．図で書くと分かりやすいんだが，そこまでの余力はないので頑張って頭で想像してくれ．それをやるのが次の CSS :

.. code-block:: css

  .tooltip .tooltip-arrow {
    width: 0;
    height: 0;
    margin: 5px;
    position: absolute;
    border-width: 5px;
    border-style: solid;
    border-color: #ffffff;
  }

  .tooltip[x-placement^="bottom"] {
    margin-top: 5px;
  }
  .tooltip[x-placement^="bottom"] .tooltip-arrow {
    border-left-color: transparent;
    border-top-color: transparent;
    border-right-color: transparent;
    top: -10px;
    left: calc(50% - 5px);
  }

``.tooltip[x-placement^="bottom"]`` で ``margin-top: 5px;`` を指定して，まず三角形を表示する領域を確保してる．で，三角形の領域の ``width`` と ``height`` を 0 にして， ``margin`` でボーダを描く領域を確保してる．後は， ``border`` 作って ``left`` と ``top`` ， ``right`` を透過させ，中心点を上に持ってくだけ．中心は，上に確保した ``margin-top: 5px;`` とボーダの長さ ``5px`` で合計 ``10px`` ずれてるため， ``-10px`` 上にすればいい．後，右に ``5px`` ずれてるので，それも戻す． ``calc(50% - 5px)`` してるのはツールチップの中心に置くため．

後いい感じに影をつける．ツールチップ本体の方は， ``box-shadow`` で適当につけてやればいいんだが，問題は三角形の方．普通に ``box-shadow`` を使うと四角形の周りに影ができるので，ツールチップ本体にかぶったり形が違ったり，色々良くない．これは，三角形の作り方を利用すればいい感じに解決できる．三角形は上部分が投下されてるので， ``overflow: hidden`` を使うと上部分だけ残してはみ出す何かは消すみたいなことができる．なので，三角形の周りにはみ出してもいいので適当に影を作って， ``overflow: hidden`` によってはみ出した部分を抹消するみたいな荒技ができる．これを使って以下のスタイルを書いた:

.. code-block:: diff

    .tooltip {
  +   box-shadow: rgba(0,0,0,0.2) 0px 0px 12px 0px;
    }

    .tooltip .tooltip-arrow {
  +   overflow: hidden;
    }
  + .tooltip .tooltip-arrow::after {
  +   content: "";
  +   position: absolute;
  +   width: 5px;
  +   height: 5px;
  +   transform: rotate(45deg);
  +   left: calc(50% - 5px);
  +   box-shadow: rgba(0,0,0,0.2) 0px -3px 12px 0px;
  +   background-color: transparent;
  + }

三角形の影は辺の長さ ``10px`` の正方形を 45 度回転させて， ``box-shadow`` で作っている [#standard-css-triangle]_ ．

まとめ
------

脚注のためのツールチップを導入した．最近，ブログの調整しかしてない気がしてきたな...

.. [#bootstrap-touch-device-detection] https://github.com/twbs/bootstrap/blob/v4.3.1/js/src/tooltip.js#L320
.. [#standard-css-triangle] というかこれが本来の三角形の作り方ではある．ただ，ツールチップ本体とかぶる部分の影のいい感じの消し方が分からんかった．
