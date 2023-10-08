ボックスタイプとCSSレイアウト
======================================

:tags: CSS, HTML, Web, デザイン
:category: 規格

さて、

.. code-block:: html
    :linenos: none

    <div>デザインにおいて、<div style="text-decoration: underline;">レイアウト</div>は重要な要素だ。</div>

という HTML 片が、基本的なブラウザでどう表示され、それがどういう原理からくるものか分かるだろうか？ そして、それが意図に反した表示になっている場合に、意図通りに直すことはできるだろうか？ これに対して明確な答えを持っている人は、この記事に書いてあることを既に理解しているであろうから引き返してなんら問題はない。

結論から言えば、これは

.. code-block:: html
    :linenos: none

    <div>デザインにおいて、<span style="text-decoration: underline;">レイアウト</span>は重要な要素だ。</div>

と書くのが一つ意図通りの HTML 片だったということになる。 ``div`` タグの代わりに、 ``span`` タグを使っているところが異なる。実は最初に提示した HTML 片は、基本的なブラウザでは「デザインにおいて、」の後と、「は重要な要素だ。」の前に改行が入るようレンダリングされる。対して、 ``span`` タグを使用すると、一行にそのまま表示される。部分的に下線をつけるためだけに ``div`` タグを使っていたつもりが、なぜかレイアウトにまで影響を与えていてしまったということだ。

この結果を引き起こす仕様はごく単純なもので、CSS を日頃触ってる人には常識ではあると思うのだが、少なくとも僕は CSS に真面目に入門するまではどういうことが起きているのかまるで分からなかった。今回は、この CSS を触ってる人には常識であろう、しかし CSS を使い慣れていない人には知らない人も多いと思われる CSS レイアウトの基本について見ていこうと思う。

通常フロー
----------------

「`CSS レイアウト入門 | MDN <https://developer.mozilla.org/ja/docs/Learn/CSS/CSS_layout/Introduction>`_」という素晴らしい入門サイトがあるので、そっち見てもらった方がいいかもしれんが、ひとまず基本から。

Web コンテンツは、コンテンツ内容を表す構造化データ HTML、そしてその構造化データの見た目、つまり配置と装飾を司る CSS で基本的に構成されている。JavaScript やその他の Web API により、ブラウザ上でコンテンツを動的に弄る術も発展しており、昨今は JavaScript から動的にコンテンツ生成する技法が主流になりつつはあるが、そこでも基本的にはコンテンツデータを HTML 片で、配置と装飾は CSS 片でが主流である。CSS というと装飾機能というイメージを持つ人は多いかもしれないが、Web コンテンツとしてはコンテンツ配置も CSS に分離されている。単に HTML 要素に対してデフォルトの配置が決まっているだけで、やろうと思えば自由に配置を弄れる。

CSS において配置を制御する最も基本的な方法が、 ``display`` プロパティによるページレイアウトである。 ``display`` プロパティは HTML 要素に対して、大きく2種類のレイアウト設定を行える:

通常フローでの要素の振る舞い (display-outside)
    レイアウト方法はいくつか種類があるが、その内通常フロー (normal flow)、またはフローレイアウトと呼ばれるデフォルトで採用されるレイアウト上で、その要素がどう振る舞うかを指定する。

内側でのレイアウト方法 (display-inside)
    その要素の子要素に対してどういうレイアウト方法を適用するかを指定する。

正確には他にもいくつかの設定を指定可能だが、そこは今回は触れない。display-outside には、後述する ``block``、 ``inline`` の2種類が指定できる。また、display-inside にはフローレイアウト ``flow`` の他いくつかのレイアウト方法を指定できるが、ひとまず ``flow`` のみ見ていく。これら2つを組み合わせて、例えば display-outside ``block``、display-inside ``flow`` の場合 ``display: block flow;`` のように指定する。

さて、CSS のレイアウトは基本的には何種類かのボックスを要素ごとに発行し、それぞれのボックスのコンテキストに従って配置を決めていく。フローレイアウトにおいては、基本的には、 ``block`` は垂直方向への配置、 ``inline`` は水平方向への配置を行う指定になる。では、その詳細を見ていこう。

まず、 ``display: block flow;`` が指定された要素は、ブロックボックスというボックスを発行する。ブロックボックスは、CSS 上でブロックレベルボックスと呼称されるものの性質を持ち、かつブロックコンテナボックスと呼ばれる性質も持つもののことだ。ブロックレベルボックスの性質として、コンテナボックス内に配置されると水平方向に伸びスペースを占有する。また、ブロックコンテナボックスの性質から、中に複数のブロックレベルボックスまたは複数のインラインレベルボックスを配置できる。つまり、その要素の外側、親要素から見た時の性質としては水平方向のスペースを占有して垂直方向に配置されるボックスとして振る舞い、要素の内側に配置される子要素からは垂直もしくは水平方向への要素配置空間を提供するボックスとして振る舞うということになる。

具体例を見る前に ``display: inline flow;`` の方も見ておこう。こちらの場合はインラインボックスというボックスを発行する。インラインボックスは、インラインレベルボックスというものの性質を持ち、かつ内側の配置方法もインライン形式を取るものになる。インラインレベルボックスの性質から、水平方向にボックスが配置される。また、インライン形式での配置により、中の要素はさらに行ボックスというものに分割される。分割は要素が水平方向になるべくはみ出ないかつ、いっぱいいっぱいになるように行われ [#split-line-boxes]_、水平方向に収まらない行ボックスは次の行に回されることで配置が行われる。

なお、ブロックボックス、インラインボックスは、適宜自動で生成されることもある。例えば、次の例を見てみよう:

.. code-block:: html

    <div style="display: block flow;">
        この HTML 片では、
        <div style="display: block flow;">ブロックボックス</div>
        と
        <div style="display: inline flow;">インラインボックス</div>
        が双方生成されます。
    </div>

この場合まず、一番外側の ``div`` 要素でブロックボックスが発行され、その中身がそのボックスに配置されることになる。そして、ブロックコンテナボックスの性質として、ブロックレベルボックスが一つでも含まれるブロックコンテナボックスでは、全ての要素がブロックレベルボックスに配置されるよう、自動でブロックボックスが発行される。具体的に上の例では、以下のようなブロックボックス生成が行われる:

.. image:: {attach}display-property-of-css/force-blocks-image.png
   :alt: 3つの生成されたブロックボックスを可視化

この場合、「``<div style="display: block flow;">ブロックボックス</div>``」がブロックレベルボックスであるため、他のテキストノードも含めた要素は、それぞれ必要に応じて自動生成されたブロックボックス内に配置される。さらに、ブロックレベルボックスを含まないブロックコンテナボックス内のテキストノードは、自動生成されたインラインボックスに配置される。これにより、次のようなインラインボックス生成が行われる:

.. image:: {attach}display-property-of-css/generated-inline-boxes-image.png
   :alt: 5つの生成されたインラインボックスを可視化

もう一つ例を見ておこう。

.. code-block:: html

    <div style="display: block flow;">
        水平方向に収まらない
        <div style="display: inline flow;">インラインレベルボックス</div>
        は、複数の
        <div style="display: inline flow;">行ボックス</div>
        に分割され、必要に応じて垂直方向への移動を挟みながら水平方向に並べられます。
    </div>

に対して、以下のようなインラインボックス生成が行われる:

.. image:: {attach}display-property-of-css/generated-inline-boxes-image-with-lines.png
   :alt: 5つの生成された行ボックスを可視化

さらに水平方向に収まらない部分は、行ボックス分割がおき、必要に応じて垂直方向への移動がおきる:

.. image:: {attach}display-property-of-css/generated-line-boxes-image.png
   :alt: 6つの生成された行ボックスを可視化

これが通常フローでの配置の基本となる。

HTML 要素のボックスタイプ
----------------------------

さて、CSS のデフォルトのレイアウト方法、通常フローもしくはフローレイアウトでは、ブロックボックス、インラインボックスの発行を指定することで配置方法を制御できることは分かってもらえただろう。これにより、最初の例はある程度説明がつく。つまり、

.. code-block:: html
    :linenos: none

    <style>
        div {
            display: block flow;
        }
        span {
            display: inline flow;
        }
    </style>
    <div>デザインにおいて、<div style="text-decoration: underline;">レイアウト</div>は重要な要素だ。</div>
    <div>デザインにおいて、<span style="text-decoration: underline;">レイアウト</span>は重要な要素だ。</div>

という解釈が行われる仕組みがあれば、 ``div`` タグを使った場合と ``span`` タグを使った場合のレイアウトの違いについて説明がつく。実はまさに、HTML Standard と CSS3 の CSS デフォルト値から、以上の挙動と同じになることが説明できる。

まず、CSS3 では ``display`` プロパティの初期値は、 ``inline`` となることが決められている [#spec-of-display-property]_。つまり、通常明示的に ``display`` プロパティを書かない場合、 ``display: inline;`` と書くのと同じ挙動になることになる。 ``display: inline;`` は詳細は後述するが、 ``display: inline flow;`` の省略形となる。よって上の例は、 ``span`` についてはわざわざ ``display: inline flow;`` と指定しなくても良いことになる。また、HTML Standard には HTML 要素に対するデフォルトのスタイルシートが規定されており [#spec-of-html-rendering]_、以下の要素はデフォルトで ``display: block;`` が指定されている:

* ``html``
* ``body``
* ``address``
* ``blockquote``
* ``center``
* ``dialog``
* ``div``
* ``figure``
* ``figcaption``
* ``footer``
* ``form``
* ``header``
* ``hr``
* ``legend``
* ``listing``
* ``main``
* ``p``
* ``plaintext``
* ``pre``
* ``search``
* ``xmp``

``display: block;`` は ``display: block flow;`` と書くのと同じ挙動になる。これらの仕様から最初に提示した振る舞いが起きることになる。

その他のレイアウト
----------------------

最後に、``display`` プロパティについてもう少し詳細を見ておこう。display-outside についてはもう見たわけだが、display-inside は ``flow`` の他にいくつかレイアウト方法を指定できる。他に指定できる種類は以下だ:

``flow-root``
    ブロックコンテナボックスを、独自の配置空間で発行し、そこに子要素を配置する。``flow`` と異なるところは、例え display-outside で ``inline`` が指定されていようとブロックコンテナボックスが発行されるところだ。

``flex``
    フレックスボックスという特殊なボックスを発行し、独自のレイアウト方法を取る。詳しくは、`フレックスボックス入門ガイド <https://developer.mozilla.org/ja/docs/Learn/CSS/CSS_layout/Flexbox>`_ を見てもらうのが良いだろう。

``grid``
    グリッドボックスという特殊なボックスを発行し、独自のレイアウト方法を取る。詳しくは、`グリッド入門ガイド <https://developer.mozilla.org/ja/docs/Learn/CSS/CSS_layout/Grids>`_ を見てもらうのが良いだろう。

``table``
    ``table`` タグと同様のレイアウトを取る。これは今回は詳しくは触れない。中々使う機会もないだろう。

``ruby``
    ``ruby`` タグと同様のレイアウトをとる。これは今回は詳しくは触れない。中々使う機会もないだろう。

これらと display-outside の組み合わせで ``display`` プロパティを指定する。なお、CSS2 との互換性と省略のため、省略形が規定されている。それぞれ次のようになる:

.. csv-table::
   :header: "省略形", "新しい値"

   "display: inside;","display: inside flow;"
   "display: block;","display: block flow;"
   "display: flow-root;","display: block flow-root;"
   "display: inline-block;","display: inline flow-root;"
   "display: flex;","display: block flex;"
   "display: inline-flex;","display: inline flex;"
   "display: grid;","display: block grid;"
   "display: inline-grid;","display: inline grid;"
   "display: ruby;","display: inline ruby;"
   "display: table;","display: block table;"
   "display: inline-table;","display: inline table;"

特に ``inline-block``、つまり ``inline flow-root`` は通常フローでも重宝される ``display`` 指定で、例えば次のような配置を実現できる:

.. code-block:: html

    <div style="display: block flow;">
        インラインブロックを使用することで、
        <div style="display: inline flow-root;">
            <div style="display: block;">ブロック1</div>
            <div style="display: block;">ブロック2</div>
        </div>
        のようにインラインレベルでブロックを二重に並べたりできます。
    </div>

.. image:: {attach}display-property-of-css/inline-block-image.png
   :alt: インラインボックスによる配置

単純にインラインボックスの中にブロックボックスを配置する、つまり

.. code-block:: html

    <div style="display: block flow;">
        インラインブロックを使用することで、
        <div style="display: inline flow;">
            <div style="display: block flow;">ブロック1</div>
            <div style="display: block flow;">ブロック2</div>
        </div>
        のようにインラインレベルでブロックを二重に並べたりできます。
    </div>

というような書き方では、内部のブロックは横一杯に広がってしまうが、 ``inline flow-root`` では独自の配置空間を作ることにより、外側から見たらインラインレベルボックスとして、内側から見たらブロックコンテナボックスとして、つまりその行の部分的な要素でありながら、内部にボックスを配置できるような振る舞いを持たせることができる。この辺の詳細を正確に知りたい場合は、CSS2 の幅・高さ計算などの仕様を見てもらうのがいいだろう。なお、 ``inline flow-root`` をデフォルトの挙動として持つ HTML 要素として、

* ``input``
* ``button``

がある。これらの振る舞いの違いやデフォルト値によって、現在の HTML の Web ブラウザでの描画配置は形作られている。

まとめ
----------

というわけで今回は、CSS の基礎ではあるが、日頃触ってない人にはあまり知られていない通常フローの配置について紹介した。通常フローでは、ブロックボックスとインラインボックスという基本的なレイアウト要素がある。また、それぞれの区分けは HTML 要素ごとにデフォルト値が決まっており、明示的に指定しない場合はその HTML の意味論に沿った配置がされる。あくまでデフォルト配置なので、配置のために HTML の意味論に反した使い方をするより、HTML の意味論に沿った使い方をしつつ明示的に配置方法を CSS で変更するのがいいだろうが、デフォルトの挙動を把握することは HTML の意味論の理解にも繋がりやすいだろう。

実はここら辺のレイアウトがちゃんと HTML / CSS で切り離されたのは結構最近だったりするし、CSS3 の ``display`` 2値指定も取り扱ってる文献はまだまだ少ない。なので、それらを整理して理解する良い機会となった。ま、何かの参考になれば嬉しい。では、今回はこれで。

.. [#split-line-boxes] ``word-break`` プロパティなどにより、ある程度分割の戦略は制御できる。
.. [#spec-of-display-property] https://www.w3.org/TR/css-display-3/#propdef-display
.. [#spec-of-html-rendering] https://html.spec.whatwg.org/multipage/rendering.html#rendering
