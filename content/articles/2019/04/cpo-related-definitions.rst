CPO に関連する定義まとめ
========================

:date: 2019-05-02 01:30
:tags: Domain Theory, 束論, 順序理論, 表示的意味論, 本
:category: 数学

某勉強会がコーヒーブレイクタイムに入って，ブレイクタイムから抜けたら何もかも忘れてそうだったので，数式レンダリングのテストも兼ねて CPO 関連の定義だけまとめておくことにした．

なお各定義は， SoPL (Semantics of Programming Languages) に則ってる．

Complete Partial Order
----------------------

まあ，まずはおなじみのやつから．

定義. 半順序 (partially ordered set, poset, partial order)
  集合 :math:`D` と二項関係 :math:`{(\sqsubseteq)} \subseteq D \times D` の組 :math:`\langle D, \sqsubseteq\rangle` で，以下の制約を満たすもの:

  反射律 (reflexive)
    :math:`\forall x \in D\ldotp x \sqsubseteq x`

  反対称律 (anti-symmetric)
    :math:`\forall x, y \in D\ldotp (x \sqsubseteq y \land y \sqsubseteq x) \implies x = y`

  推移律 (transitive)
    :math:`\forall x, y, z \in D\ldotp (x \sqsubseteq y \land y \sqsubseteq z) \implies x \sqsubseteq z`

なお，二項関係が文脈から分かる場合単に集合を poset と呼んだり，逆に集合が分かる場合二項関係を半順序と呼んだりする．二項関係がどの poset のものか示すため， :math:`\sqsubseteq_D` と書いたりもする．total な関係になっていれば，全順序．

定義. (上に) 有界 (bounded (above), consistent)
  poset :math:`D` に対して，その部分集合 :math:`P \subseteq D` が (上に) 有界とは，以下を満たすこと:

  .. math::
    \exists x \in D\ldotp \forall y \in P\ldotp y \subseteq x

  この時， :math:`x` を :math:`P` の上界 (an upper bound) と呼ぶ．

なお，普通 bounded は上下に有界な時使う気がするけど，この後は特に断りない限り， bounded above のことを有界と言っていく．後逆は， bounded below と lower bound ．それと， :math:`\{x_1, \ldots, x_n\}` が有界な時， :math:`x_1 \uparrow \cdots \uparrow x_n` と書く．組になってる時は， consistency pair と呼んだりもするっぽい．

定義. 上限 (least upper bound, lub, supremum, sup)
  poset :math:`D` ，その有界集合 :math:`P \subseteq D` に対して，その上界 :math:`\bigsqcup P \in P` が上限とは，以下を満たすこと:

  .. math::
    \text{任意の上界 \(x\) について}\,\bigsqcup P \sqsubseteq x

:math:`\mathop{\mathrm{sup}} P = \bigsqcup P` という記法や， pointwise に :math:`\bigsqcup_{x \in P} x = \bigsqcup P` ，:math:`\mathop{\mathrm{sup}}_{x \in P} x = \mathop{\mathrm{sup}} P` という記法を使うこともある．また， :math:`x_1 \sqcup \cdots \sqcup x_n = \bigsqcup \{x_1, \ldots, x_n\}` と書くこともある．

上限の中で一番小さいやつ．全ての有界集合が sup 持つとは限らない (上界同士で上下関係がないことがあるので) ことに注意．存在すれば一意に決まる．逆は， greatest lower bound (glb) または infimum (inf) ．

定義. 有向部分集合 (directed subset)
  poset :math:`D` に対して，その部分集合 :math:`M \subseteq D` が有向とは，以下を満たすこと:

  .. math::
    \forall U \in \mathcal{P}_{\mathit{fin}}(M)\ldotp \text{\(U\) は上界 \(x \in P\) を持つ}

名前の意味的には，部分集合の上界を集めるとまたそれに上界があり，それにもやっぱり上界があってと言う感じで，最終的にある場所に対して上に上に順序が伸びていく感じ．なお，伸びていく場所が 2 点とか 3 点になってるような奴は， directed にならない．あと空集合にも上界の存在を求めてるので，空集合自体は directed じゃないことにも注意．また， directed subset は自身の内に上限を持つとは限らなくて， :math:`[0,1) \subseteq \mathbb{R}` を考えてみると，こいつは poset :math:`\langle \mathbb{R}, \leq\rangle` の directed subset になってて，上限は 1 だが，これは自身の内にはない．

定義. 完備半順序 (complete partial order, cpo)
  poset :math:`D` が，完備であるとは，以下を満たすこと:

  .. math::
    \text{任意の directed subset \(P \subseteq D\) は sup を持つ}

directed subset は，有限の場合 sup を中に持つ．なので，有限 poset は cpo になる．cpo でない例としては，自然数の集合に通常の順序 :math:`\leq` をいれた poset :math:`\omega = \{0, 1, \ldots\}` とか． :math:`\omega` は自身の directed subset になるが，その上限は存在しない．で， :math:`\omega_{\infty} = \omega \cup \{\infty\}` に拡張して :math:`\infty` を最大要素となるよう順序も拡張すると，こいつは cpo になる．

定義. 点付き (pointed)
  poset :math:`D` が点付きであるとは，以下を満たすこと:

  .. math::
    \exists \bot \in D\ldotp \forall x \in D\ldotp \bot \sqsubseteq x

CPO の性質
----------

こっちもおなじみのやつ．

定義. 単調 (monotone)
  poset :math:`D` から :math:`E` への関数 :math:`f: D \to E` が単調とは，以下を満たすこと:

  .. math::
    \forall x, y \in D\ldotp x \sqsubseteq y \implies f(x) \sqsubseteq f(y)

順序を保存する関数．流石に順序ぐらいは保存してくれないとね．

定義. 連続 (continuous)
  poset :math:`D` から :math:`E` への単調関数 :math:`f: D \to E` が連続とは，以下を満たすこと:

  .. math::
    \text{任意の directed subset \(P \subseteq D\) について}\, f(\bigsqcup P) = \bigsqcup_{x \in P} f(x)

なお，有限だけ考える場合の連続性だけなら単調性だけから言えて，directed subset :math:`P = \{x_i\}_{1 \leq i \leq n}` において， :math:`x = \bigsqcup P \in P` となるものがあり， :math:`\forall i\ldotp x_i \sqsubseteq x` より :math:`\forall i\ldotp f(x_i) \sqsubseteq f(x)` なので， :math:`\bigsqcup_i f(x_i) = f(x) = f(\bigsqcup_i x_i)` となる．なお， pointed cpo に対して連続関数から不動点を作れる (不動点定理)．

定義. 正格 (strict)
  pointed poset :math:`D` から :math:`E` への関数 :math:`f: D \to E` が正格とは，以下を満たすこと:

  .. math::
    f(\bot) = \bot

pointed を保存する関数．

定義. :math:`\omega`-完備半順序 (:math:`\omega`-cpo)
  poset :math:`D` に対して，

  * 単調関数 :math:`f: \omega \to D \simeq (x_n \in D)_{n \in \omega}` を :math:`\omega`-chain と呼ぶ．
  * 任意の :math:`\omega`-chain :math:`(x_n)_{n \in \omega}` が sup を持つ時， :math:`D` を :math:`\omega`-cpo と呼ぶ．

  また， :math:`\omega`-cpo :math:`D` から :math:`E` への単調関数 :math:`f: D \to E` が :math:`\omega`-continuous とは，以下を満たすこと:

  .. math::
    \text{任意の \(\omega\)-chain \((x_n)_{n \in \omega}\) について}\,f(\bigsqcup_{n \in \omega} x_n) = \bigsqcup_{n \in \omega} f(x_n)

:math:`\omega`-chain はつまり可算な増加列のこと．こいつは有限部分集合を取ると明らかに最大要素が一つ確定するため有界であり， directed subset になる．なので， cpo は :math:`\omega`-cpo になる．ただ，その逆は成り立たないらしい [#cpo-vs-omega-cpo]_． :math:`\omega`-cpo でも不動点定理が成り立つ．

定理. cpo と連続関数による圏は，CCC
  Cpo を cpo と連続関数から作られる圏とする．この時， Cpo は Cartesian Closed．

  証明:

  terminal object
    .. math::
      1 = \{*\}

    1 要素の cpo への関数は必ず連続になる．

  product
    .. math::
      \forall x_1 \sqsubseteq x_2 \in D, y_1 \sqsubseteq y_2 \in E\ldotp (x_1, y_1) \sqsubseteq (x_2, y_2) \in D \times E

    product order が単純に直積になる．

  exponential object
    .. math::
      D^E = \{f \mid \text{連続関数 \(f: D \to E\)}\}

    pointwise order (:math:`f \sqsubseteq g \iff \forall x \in D\ldotp f(x) \sqsubseteq g(x)`) 入れた連続関数空間が冪になる．単純に :math:`\mathit{apply}(f, x) = f(x)` / :math:`\mathit{curry}(f)(x)(y) = f(x, y)` は連続関数になる．

ドメイン
--------

こっからが本命みたいなとこがある．

定義. コンパクト (compact)
  cpo :math:`D` に対して， :math:`x \in D` がコンパクトとは，以下を満たすこと:

  .. math::
    \text{任意の directed subset \(M \subseteq D\) について}\,x \sqsubseteq \bigsqcup M \implies \exists y \in M\ldotp x \sqsubseteq y

  なお， :math:`D` の compact elements 全体を :math:`\mathrm{K}(D)` と書く．

cpo の場合のコンパクト性について． compact element は finite element とも呼ばれ，自身を近似するやつ．一般に，

.. math::

  x \ll y \iff (\forall M \subseteq D\ldotp \text{\(M\) は directed} \land y \sqsubseteq \bigsqcup M \implies \exists a \in M\ldotp x \sqsubseteq a)

を近似関係と言って， :math:`x` は :math:`y` を近似すると読む．

定義. algebraic
  cpo :math:`D` が algebraic とは，以下を満たすこと:

  .. math::
    \forall x \in D, M = \{a \in \mathrm{K}(D) \mid a \sqsubseteq x\}\ldotp \text{\(M\) は directed} \land \bigsqcup M = x

algebraic cpo のことを domain と呼ぶことにする． domain 内の要素はそれ以下の compact elements の sup で表せる．これは色々便利な性質だけど，その圏は CCC にならない．具体的には，冪が作れない．

定義. 基底 (basis)
  cpo :math:`D` に対して， :math:`D_0 \subseteq \mathrm{K}(D)` が :math:`D` の基底を成すとは，以下を満たすこと:

  .. math::
    \forall x \in D, M = \{a \in D_0 \mid a \subseteq x\}\ldotp \text{\(M\) は directed} \land \bigsqcup M = x

なお， :math:`D_0` が :math:`D` の基底を成す時， :math:`D` は algebraic で :math:`\mathrm{K}(D) = D_0` となる． basis を持つ，つまり algebraic の範囲では cpo と :math:`\omega`-cpo は一致するらしい [#cpo-vs-omega-cpo]_．

定義. 完備束 (complete lattice)
  poset :math:`D` が完備束とは，以下を満たすこと:

  .. math::
    \forall M \subseteq D\ldotp \bigsqcup M \in D

なお，この定義は完備半束と呼ばれるやつで，下限でもいい．通常の完備束は，上限下限どちらも要求する．ただ，完備半束は完備束と一致する．つまり，どちらかがあればどちらもある．

定理. 完備半束と完備束の一致
  poset :math:`D` において，以下は同値:

  1. 任意の部分集合 :math:`M \subseteq D` は上限を持つ (完備束)
  2. 任意の部分集合 :math:`M \subseteq D` は下限を持つ
  3. 任意の部分集合 :math:`M \subseteq D` は上限下限を持つ

  証明:

  3 :math:`\implies` 1 / 3 :math:`\implies` 2 / 1 かつ 2 :math:`\implies` 3 はいいので， 1 :math:`\implies` 2 / 2 :math:`\implies` 1 が示せればいい．どちらか片方が示せれば，もう片方は対称的に証明可能なので， 1 :math:`\implies` 2 だけ示す．

  まず，空集合を考えるとこいつにも上限があるはずで，そいつは最小元になる．

  最小元は，全ての部分集合の下界になるため，少なくとも下界は一つは存在する．任意の部分集合 :math:`M` の下界全体の集合を :math:`\mathop{\downarrow} M \neq \emptyset` と書くとする．この時， :math:`M` の要素は :math:`\mathop{\downarrow} M` の上界になり，また :math:`\bigsqcup (\mathop{\downarrow} M)` が存在するはずで，こいつは以下を満たす:

  .. math::

    \left\{\begin{array}{l}
      \forall x \in M\ldotp \bigsqcup (\mathop{\downarrow} M) \sqsubseteq x \\
      \forall x \in \mathop{\downarrow} M\ldotp \mathop{\downarrow} M \sqsubseteq \bigsqcup (\mathop{\downarrow} M)
    \end{array}\right.

  つまり， :math:`\bigsqcup (\mathop{\downarrow} M)` は :math:`M` の下界であり，かつ下界の最大要素であるため，下限となる．

  以上より，定理が示せる．

一応束論ってタグつけたので，ちょっとは束論らしいことやらないとね？ complete lattice は cpo になる．

定義. 有界完備 (bounded complete)
  空でない cpo :math:`D` が有界完備とは，以下を満たすこと:

  .. math::
    \text{任意の有界集合 \(M \subseteq D\) が \(\bigsqcup M \in D\) を持つ}

なお， cpo は directed complete な poset と呼ばれることがあり， directed + bounded complete な poset が， bounded complete cpo になる．bounded complete な domain を， bc-domain と呼ぶことにする． bc-domain による圏は CCC になる．

補題. 点付き有界完備 cpo の同値条件
  pointed cpo :math:`D` において，以下は同値:

  1. :math:`D` は bounded complete
  2. 任意の :math:`x \uparrow y` について， :math:`x \sqcup y \in D`

  証明:

  1 :math:`\implies` 2 は，有界集合として :math:`M = \{x, y\}` を取れば自明に成り立つ．

  2 :math:`\implies` 1 は，以下のように示せる [#helped-proof]_．

  任意の有界部分集合 :math:`M \subseteq D` について， :math:`M' \in \mathcal{P}_{\mathit{fin}}(M)` は有界で有限なので， :math:`\bot` から各要素の sup 取りまくれば全体の sup が作れる．

  ここで， :math:`N = \{\bigsqcup M' \mid M' \in \mathcal{P}_{\mathit{fin}}(M)\}` を考える．こいつは，任意の :math:`U \in \mathcal{P}_{\mathit{fin}}(N)` に対して， :math:`\bigsqcup \bigcup \{M' \mid \bigsqcup M' \in U\} \in U` が上界となるので， :math:`U` は directed set になる． :math:`D` は cpo なので， :math:`\bigsqcup U \in D` が存在する．

  :math:`\bigsqcup U` は :math:`M` の上界であり，他の任意の上界は :math:`U` の上界にもなるので， :math:`\bigsqcup M = \bigsqcup U` となる．よって，題意は成り立つ．

2 の逆は sup があれば有界なことより自明なので， :math:`x \mathrel{\uparrow} y` と :math:`x \sqcup y \in D` は bc-domain では同値条件になる．

定義. 単項イデアル (principal ideal)
  poset :math:`P` ， :math:`x \in P` について， :math:`x` により生成される単項イデアルとは，集合 :math:`\downarrow x = \{y \in P \mid y \sqsubseteq x\}` のこと．

定義. property I
  algebraic cpo :math:`D` が property I を持つとは，以下を満たすこと:

  .. math::

    \forall a \in \mathrm{K}(D)\ldotp \text{\(\downarrow a\) は有限}

property I + bounded complete な domain では，有限個で要素を近似できる．ただ， CCC は作れない．これはやっぱり冪が作れないから．単なる連続関数空間が property I を持たないため．

定義. 分配的 (distributive)
  bc-domain :math:`D` が分配的とは，以下を満たすこと:

  .. math::

    \forall x, y \uparrow z \in D\ldotp x \sqcap (y \sqcup z) = (x \sqcap y) \sqcup (x \sqcap z)

distributive で property I を持つ bc-domain を dI-domain と呼ぶことにする．

定義. stable
  dI-domain :math:`D` から :math:`E` への連続関数 :math:`f: D \to E` が， stable とは，以下を満たすこと:

  .. math::

    \forall x \uparrow y\ldotp f(x \sqcap y) = f(x) \sqcap f(y)

dI-domain と stable function による圏は CCC になる．冪は，以下の stable order で作る:

.. math::

  f \sqsubseteq_s g \iff \forall x \sqsubseteq y\ldotp f(x) = f(y) \sqcap g(x)

まとめ
------

色々数式環境お試しのために書いた． :math:`\KaTeX` では， ``\bigsqcap`` が書けないこととかが分かった．

これで，某勉強会のコーヒーブレイクタイムが終わっても，元の話題をちゃんと思い出せるといいな．

.. [#cpo-vs-omega-cpo] https://en.wikipedia.org/wiki/Complete_partial_order#Definitions
.. [#helped-proof] これ自力じゃ証明できなくて，某勉強会の人に教えてもらった．
