始代数と終余代数が一致する条件
==============================

:date: 2019-12-16 20:00
:tags: 圏論, cpo, F-algebra
:category: 数学

http://ziphil.com/diary/application/26.html を読んでるとき，まとめたことを記事として残しておこうと思った．

FIXME
  なお，主定理の一つを証明できてないし，原論文の証明も理解できてない． Barr 先生何言ってんのって感じ．誰か，俺に圏論の常識を教えてくれ．

参考文献は，

  Barr, M. (1992). Algebraically compact functors. Journal of Pure and Applied Algebra, 82(3), 211–231. https://doi.org/10.1016/0022-4049(92)90169-G

なお以降考える圏は，特に断りのない限り， initial object 0 及び terminal object 1 が存在し， chain cocomplete (chain に対する図式が colimit を持つ) で cochain complete (cochain に対する図式が limit を持つ) であるとする．

F-algebra と F-coalgebra
------------------------

圏 :math:`C` ，自己関手 :math:`F: C \to C` において， :math:`F`-algebra は，射 :math:`F X \xrightarrow{\alpha} X` のこと．で， :math:`F`-algebra 同士の射を以下の図式を可換にする射 ( :math:`F`-algebra 準同型と呼ぶ)

.. image:: {attach}algebraic-compact-functor/f-algebra-homomorphism.png
  :alt: :math:`F X_1 \xrightarrow{\alpha_1} X_1` ，:math:`F X_2 \xrightarrow{\alpha_2} X_2` に対して， :math:`f \circ \alpha_1 = \alpha_2 \circ F f` を満たす :math:`X_1 \xrightarrow{f} X_2`
  :align: center

とすると， :math:`F`-algebra による圏 :math:`\mathrm{Alg}_C(F)` を構成できる．この圏で initial object が存在したとき，それを initial :math:`F`-algebra と呼ぶ．その双対をそれぞれ :math:`F`-coalgebra ， :math:`\mathrm{CoAlg}_C(F)` ，terminal :math:`F`-coalgebra と呼ぶ．

ところで， initial algebra :math:`F A \xrightarrow{\alpha} A` において， :math:`F A \sim A` であることが知られている．

定理. (Lambek's Theorem)
  自己関手 :math:`F: C \to C` において，initial :math:`F`-algebra :math:`F A \xrightarrow{\alpha} A` があるとする．この時， :math:`\alpha` は同型射．

  証明:

  以下の図式は可換:

  .. image:: {attach}algebraic-compact-functor/initial-f-algebra-iso.png
    :alt: initial algebra に :math:`F` を適用したものを介した， initial algebra から initial algebra への合成射
    :align: center

  よって， initial algebra からの射の一意性から， :math:`\beta; \alpha = \mathrm{id}` ．さらに，左の図式だけ考えると :math:`\alpha; \beta = F \beta; F \alpha = F (\beta; \alpha) = F\, \mathrm{id} = \mathrm{id}` より， :math:`\alpha` は同型射になる．

双対的に， terminal :math:`F`-coalgebra についても同型であることが示せる．ところで， initial / terminal object から initial algebra / terminal coalgebra を構成できる場合がある．

定義. (initial / terminal :math:`F`-sequence)
  以下の自然数順序集合からの図式を initial :math:`F`-sequence と呼ぶ．

  .. math::

    0 \xrightarrow{\exists ! f} F 0 \xrightarrow{F f} \cdots \xrightarrow{F^{n - 1} f} F^n 0 \xrightarrow{F^n f} \cdots

  この時， :math:`F^\infty 0 \sim \mathop{\mathrm{colim}}_n F^n 0` で定める．さらに， :math:`m \leq n` において， :math:`f^m_n: F^m 0 \to F^n 0` を以下のように定める:

  * :math:`f_n^n = \mathrm{id}`
  * :math:`m < n` の時， :math:`f^m_n = F^m f; f^m_n`
  * :math:`f^m_\infty` は :math:`F^\infty 0` の構成射

  また，以下の自然数順序集合からの反変図式を terminal :math:`F`-sequence と呼ぶ．

  .. math::

    1 \xleftarrow{\exists ! g} F 1 \xleftarrow{F g} \cdots \xleftarrow{F^{n - 1} g} F^n 1 \xleftarrow{F^n g} \cdots

  この時， :math:`F^\infty 1` ， :math:`m \geq n` についての :math:`g_m^n: F^m 1 \to F^n 1` を同様に定める．

命題.
  任意の :math:`l \in \mathbb{N}` について， :math:`F^\infty 0 \sim \mathop{\mathrm{colim}}_n F^{n + l} 0` ．

  証明:

  cocone :math:`(F^{n + l} 0 \xrightarrow{\alpha_n} X)_n` を考える．この時，

  .. math::

    \alpha'_n: F^n 0 \to X = f^n_{n + l}; \alpha_n

  は cocone になり，任意の :math:`n \in \mathbb{N}` で :math:`\alpha'_{n + l} = f^{n + l}_{n + 2l}; \alpha_{n + l} = \alpha_n` になる．

  ところで，任意の :math:`n \in \mathbb{N}` で，以下の図式を可換にする :math:`\alpha'_\infty: F^\infty 0 \to X` があった時，

  .. image:: {attach}algebraic-compact-functor/offset-colimit.png
    :alt: つまり， :math:`\alpha'_\infty` は分解射
    :align: center

  :math:`f^n_\infty; \alpha'_\infty = f^n_{n + l}; f^{n + l}_\infty; \alpha'_\infty = f^n_{n + l}; \alpha_n = \alpha'_n` より， cocone :math:`(\alpha'_n)_n` の分解射でもあるが，普遍射の一意性からそのような :math:`\alpha'_\infty` は一意になる．

補題. (initial algebra の構成)
  :math:`F (F^\infty 0) \sim F^\infty 0` の時，これは initial algebra

  証明:

  任意の algebra :math:`F X \xrightarrow{\alpha} X` について，

  .. math::

    h^n_\infty: F^n 0 \to X = \left\{\begin{array}{ll}
      0 \xrightarrow{!} X &(n = 0) \\
      F^n 0 \xrightarrow{F h^m_\infty} F X \xrightarrow{\alpha} X &(n = m + 1)
    \end{array}\right.

  は cocone になる．よって， :math:`F^\infty 0` からこの cocone への普遍射が存在し，これは :math:`F (F^\infty 0) \sim F^\infty 0` から :math:`F X \xrightarrow{\alpha} X` への準同型射にもなる．ところで，準同型射 :math:`h: F^\infty 0 \to X` があった時，

  .. image:: {attach}algebraic-compact-functor/initial-algebra-construction.png
    :alt: つまり， :math:`h` は分解射
    :align: center

  となることが数学的帰納法で示せる．つまり， cocone :math:`(h^n_\infty)_n` の分解射であるが，普遍射の一意性からそのような :math:`h` は一意になる．

双対的に， :math:`F^\infty 1 \sim F (F^\infty 1)` の時，これは terminal coalgebra になる．で， :math:`F (F^\infty 0) \sim F^\infty 0` となる具体的な設定として，例えば :math:`F` が colimit を保存すれば良い．

定理. (Adámek's Theorem)
  :math:`F: C \to C` が colimit を保存する時，同型射 :math:`F (F^\infty 0) \sim F^\infty 0` が存在し， initial algebra

  証明:

  :math:`F^\infty 0 \sim \mathop{\mathrm{colim}}_n F^{n + 1} 0 \sim F (F^\infty 0)` より．

双対的に terminal coalgebra も， :math:`F: C \to C` が limit を保存する時構成できる．ここまでが基礎知識で，ここからが本題．

まず， algebra から coalgebra への準同型射を以下のように定義する．

定義. (relational :math:`F`-morphism)
  :math:`F`-algebra :math:`F A \xrightarrow{\alpha} A` 及び :math:`F`-coalgebra :math:`B \xrightarrow{\beta} F B` について，以下の図式を満たす :math:`g: A \to B` を relational :math:`F`-morphism と呼ぶ:

  .. image:: {attach}algebraic-compact-functor/relational-f-morphism.png
    :alt: :math:`\alpha; g; \beta = F g`
    :align: center

自明な relational morphism として以下のものが考えられる．

定義. fixed object
  圏 :math:`C` の自己関手 :math:`F: C \to C` を考える．対象 :math:`A \in |C|` が， :math:`A \sim F A` を持つ時， :math:`A` を :math:`F` における fixed object と呼ぶ．

系.
  initial algebra 及び terminal coalgebra は fixed object

系.
  fixed object :math:`A` において， :math:`\mathrm{id}: A \to A` は algebra :math:`F A \sim A` から coalgebra :math:`A \sim F A` への relational morphism

また， initial algebra から terminal coalgebra への relational morphism は一意になる．

命題.
  initial :math:`F`-algebra から terminal :math:`F`-coalgebra への relational morphism は存在して一意．

  証明:

  terminal :math:`F`-coalgebra :math:`B \sim F B` について， :math:`F B \sim B` は algebra より， initial algebra からの準同型射が存在し，これは :math:`B \sim F B` への relational morphism にもなる．また， initial algebra からの relational morphism を持ってくると，それは :math:`F B \sim B` への準同型射でもあるので，準同型射の一意性から一意になる．

さて， initial algebra と terminal coalgebra が一致するというのは，つまりその構成 object が同型になるということだが，この時 relational morphism としてその同型射を持ってくることができる．よって，上の relational morphism の一意性から， initial algebra と terminal coalgebra の一致を以下のように言い換えできる．

定義. (canonical isomorphic)
  initial algebra から terminal coalgebra の relational morphism が同型射の時， initial algebra と terminal algebra は canonical isomorphic であるといい，その時の relational morphism を canonical isomorphism と呼ぶ．

ところで， initial sequence の colimit と terminal sequence の limit が一致し，これらから initial algebra または terminal coalgebra が構成できる時，それらは canonical iso である．

命題.
  :math:`F (F^\infty 0) \sim F^\infty 0` かつ :math:`F^\infty 0 \sim F^\infty 1` の時， :math:`F^\infty 1 \sim F (F^\infty 1)` かつこれらは canonical isomorphic

  証明:

  :math:`F^\infty 1 \sim F^\infty 0 \sim F (F^\infty 0) \sim F (F^\infty 1)` より．

双対的に terminal coalgebra が構成できる場合も同様になる．直感的には，上の canonical iso は :math:`F^n (0 \xrightarrow{!} 1)` を :math:`\infty` まで飛ばすと構成できそうである．これを示す．

定義. (relational morphism from initial sequence to terminal sequence)
  :math:`h^n_m: F^n 0 \to F^m 1` を以下のように定義する:

  * :math:`n, m \in \mathbb{N}` について，

    .. math::

      h^n_m = \left\{\begin{array}{ll}
        f^n_m; h^m_m &(n < m) \\
        F^n (0 \xrightarrow{!} 1) &(n = m) \\
        h^n_n; g^n_m &(n > m)
      \end{array}\right.

  * :math:`h^\infty_m` は :math:`F^\infty 0` から cocone :math:`(h^n_m)_n` への普遍射
  * :math:`h^n_\infty` は cone :math:`(h^n_m)_m` から :math:`F^\infty 1` への普遍射

  なお， :math:`h^\infty_\infty` は一意性からどちらの場合で作っても等しい．

命題.
  :math:`F (F^\infty 0) \sim F^\infty 0` かつ :math:`F^\infty 1 \sim F (F^\infty 1)` の時， :math:`h^\infty_\infty` はその間の relational morphism

  証明:

  以下の図式が成り立つ:

  .. image:: {attach}algebraic-compact-functor/relational-morphism-for-ini-ter-seq.png
    :alt: :math:`f^{n + 1}_\infty; \sim; F h^\infty_\infty; \sim = F f^n_\infty; F h^\infty_\infty; \sim = F h^n_\infty; \sim = h^{n + 1}_\infty`
    :align: center

  よって， :math:`F^\infty 0 \sim F (F^\infty 0) \xrightarrow{F h^\infty_\infty} F (F^\infty 1) \sim F^\infty 1` は分解射となり，一意性より :math:`h^\infty_\infty` と等しい．

ここまでが準備．

Algebraically Compact
---------------------

initial algebra と terminal coalgebra が一致するような functor を， algebraically compact と呼ぶ．

定義. (algebraically compact functor)
  圏 :math:`C` に対して，自己関手 :math:`F: C \to C` が initial :math:`F`-algebra と terminal :math:`F`-algebra を持ち， canonical isomorphic になる時， :math:`F` は algebraically compact だと呼ぶ．また， :math:`F` が fixed object を持つならば algebraically compact である時，条件付き algebraically compact であると呼ぶ．

ところで， initial algebra や terminal coalgebra は fixed object なので， fixed object がないというのはつまり，関手が initial algebra や terminal coalgebra をそもそも作れる構造を持っていないということになる．つまり，条件付き algebraically compact とは，関手がそもそも initial algebra や terminal coalgebra を持てる構造にある前提で，その一致性があるというものになる．前の系を思い出すと， fixed object があれば relational morphism は作れるので，後重要なのは initial sequence の colimit と terminal sequence の limit が一致するかということになる．なお，自明だが algebraically compact なら条件付き algebraically compact である．

ついでに， category に対してのざっくりとした algebraically compact 性も定められている．

定義. (algebraically compact category)
  圏 :math:`C` に対して，任意の自己関手 :math:`F: C \to C` が algebraically compact である時， :math:`C` を algebraically compact と呼ぶ．また，任意の fixed object を持つ :math:`F: C \to C` が algebraically compact である時， :math:`C` を条件付き algebraically compact であると呼ぶ．

定義. (algebraically complete category)
  圏 :math:`C` に対して，任意の自己関手 :math:`F: C \to C` が initial :math:`F`-algebra を持つ時， :math:`C` を algebraically complete と呼ぶ．

algebraically complete というのは Fleyd が導入した言葉 [#fleyd-1991]_ ．なお， algebraically compact category は algebraically complete category ．さて， algebraically compact になる自明な条件として，以下のものがある．

系.
  :math:`F: C \to C` について， :math:`F (F^\infty 0) \sim F^\infty 0 \sim F^\infty 1` の時， :math:`F` は algebraically compact

では，他に具体的にどういう条件下だと algebraically compact になるんだろうか？ 一つの条件としては，以下のものがある．

定理. (algebraically compact の十分条件)
  :math:`F: C \to C` について， :math:`h^\infty_\infty: F^\infty 0 \to F^\infty 1` が同型射であり，ある algebra :math:`F A \xrightarrow{\alpha} A` 及び coalgebra :math:`B \xrightarrow{\beta} F B` の間の relational morphism :math:`A \xrightarrow{h} B` が存在する時， :math:`F` は algebraically compact

  証明 (FIXME [#fixme-proof]_):

  cocone :math:`(k_n: F^n 0 \to A)_n` を以下のように定義する:

  .. math::

    k_n = \left\{\begin{array}{ll}
      0 \xrightarrow{!} A &(n = 0) \\
      F^n 0 \xrightarrow{F k_m} F A \xrightarrow{\alpha} A &(n = m + 1)
    \end{array}\right.

  同様に cone :math:`(l_n: B \to F^n 1)_n` を定義する．この時， :math:`h^n_n = k_n; h; l_n` となることが以下のように数学的帰納法で示せる:

  * :math:`n = 0` の時，一意性より :math:`h^0_0: 0 \to 1 = k_0; h; l_0`
  * :math:`n = m + 1` の時，以下が可換になる:

    .. image:: {attach}algebraic-compact-functor/canonical-iso-from-relational-morphism.png
      :alt: :math:`F k_m; F h; F l_m = F k_m; \alpha; h; \beta; F l_m = k_n; h; l_n`
      :align: center

    よって， :math:`h^n_n = F h^m_m = F (k_m; h; l_m) = F k_m; F h; F l_m = k_n; h; l_n`

  また，任意の :math:`n, m \in \mathbb{N}` について，

  .. math::

    h^n_m = \left\{\begin{array}{ll}
      f^n_m; h^m_m = f^n_m; k_m; h; l_m = k_n; h; l_m &(n < m) \\
      k_n; h; l_n &(n = m) \\
      h^n_n; g^n_m = k_n; h; l_n; g^n_m = k_n; h; l_m &(n > m)
    \end{array}\right. = k_n; h; l_m

  ここで，以下の図式を考える:

  .. image:: {attach}algebraic-compact-functor/canonical-iso-infty-from-relational-morphism.png
    :alt: :math:`h^n_\infty = f^n_\infty; k_\infty; h; l_\infty` ．つまり， :math:`k_\infty; h; l_\infty` は :math:`h^n_\infty` の分解射
    :align: center

  これは普遍性より， :math:`k_\infty: F^\infty 0 \to A` ， :math:`l_\infty: B \to F^\infty 1` が存在して可換になり，つまり :math:`k_\infty; h; l_\infty` は :math:`h^n_\infty` の分解射になる．よって，一意性から :math:`h^\infty_\infty = k_\infty; h; l_\infty` になる．この時， :math:`F (F^\infty 0) \sim F^\infty 0` であり，同型射は，以下の図式において， :math:`\mu: F^\infty 0 \to F (F^\infty 0)` 及び :math:`(F h^\infty_\infty; \eta; {h^\infty_\infty}^{-1}): F (F^\infty 0) \to F^\infty 0` で作れる．

  .. image:: {attach}algebraic-compact-functor/initial-algebra-from-relational-morphism.png
    :alt: :math:`\mu` は :math:`F^\infty 0` から cocone :math:`(F f^n_\infty)_n` への普遍射で， :math:`\eta` はその双対
    :align: center

  これが同型であることは，以下のように示せる (らしい):

  .. image:: {attach}algebraic-compact-functor/finally-canonical-iso-from-relational-morphism.png
    :alt: TODO
    :align: center

  この真ん中が可換，つまり :math:`h; l_\infty; {h^\infty_\infty}^{-1}; k_\infty; h = h; \beta; F l_\infty; F {h^\infty_\infty}^{-1}; F k_\infty; \alpha; h` が成り立てば同型になるが，これが示せねえ．誰か助けてくれ．

  FIXME
    :math:`h; l_\infty; {h^\infty_\infty}^{-1}; k_\infty` ， :math:`h; \beta; F l_\infty; F {h^\infty_\infty}^{-1}; F k_\infty; \alpha` が両方 idempotent であるということを利用するらしいが，どうやるのか分からん．

FIXME
  この後の補題やら定理の大部分は，この定理に依存してる．なので，この定理の証明は非常に重要なんだけど，できない...誰か助けてくれ．

ところで，ここから条件付き algebraically compact の条件が以下のようになることも分かる．

系. (条件付き algebraically compact の十分条件)
  :math:`F: C \to C` について， :math:`h^\infty_\infty: F^\infty 0 \to F^\infty 1` が同型射である時， :math:`F` は条件付き algebraically compact

  証明:

  fixed object の relational morphism が取れるため．

つまり，ある functor が fixed object を持つ，つまり initial algebra や terminal coalgebra を持てる構造になっていた時， initial sequence から terminal sequence の対応が同型射に落とし込める状況であればいいということになる．

具体例
------

では， initial algebra から terminal coalgebra への対応が同型になる状況は具体的にどういう状況なのかを見ていく．

補題.
  CPO enriched な圏 :math:`C` ，自己関手 :math:`C` (CPO enriched とは限らない) において，以下を満たす :math:`(F^n 1 \xrightarrow{l^n_{n + 1}} F^{n + 1} 0)_{n \in \mathbb{N}}` が存在するとする:

  .. image:: {attach}algebraic-compact-functor/morphism-from-terminal-to-initial-seq.png
    :alt: :math:`l^n_{n + 1}: F^n 1 \to F^{n + 1} 0`
    :align: center

  * 任意の :math:`n \in \mathbb{N}` で， :math:`h^n_n; l^n_{n + 1} = f^n_{n + 1}`
  * 任意の :math:`n \in \mathbb{N}` で， :math:`g^{n + 1}_n; l^n_{n + 1}; h^{n + 1}_{n + 1} \sqsubseteq \mathrm{id}`
  * 任意の :math:`n \in \mathbb{N}` で， :math:`l^n_{n + 1}; h^{n + 1}_{n + 1}; g^{n + 1}_n = \mathrm{id}`

  この時， :math:`F^\infty 0 \sim F^\infty 1`

  証明:

  :math:`F^\infty 0` が terminal sequence の limit であることを示せば， limit の一意性から言える．さて， cone :math:`(\alpha_n: X \to F^n 1)_n` を取ってきたとき，この cone から :math:`(h^\infty_\infty; g^\infty_n: F^\infty 0 \to F^n 1)_n` への普遍射が :math:`\alpha_\infty = \bigsqcup_m \alpha_m; l^m_{m + 1}; f^{m + 1}_\infty` であることを示す．

  さて，まず任意の :math:`n \in \mathbb{N}` に対して :math:`\alpha_\infty = \bigsqcup_{m > n} \alpha_m; l^m_{m + 1}; f^{m + 1}_\infty` ，つまり :math:`(\alpha_m; l^m_{m + 1}; f^{m + 1}_\infty)_m` が単調増加であることを示す．

  .. math::

    \begin{array}{ll}
      \alpha_m; l^m_{m + 1}; f^{m + 1}_\infty
      &= \alpha_{m + 1}; g^{m + 1}_m; l^m_{m + 1}; f^{m + 1}_{m + 2}; f^{m + 2}_\infty \\
      &= \alpha_{m + 1}; g^{m + 1}_m; l^m_{m + 1}; h^{m + 1}_{m + 1}; l^{m + 1}_{m + 2}; f^{m + 2}_\infty \\
      &\sqsubseteq \alpha_{m + 1}; \mathrm{id} ;l^{m + 1}_{m + 2}; f^{m + 2}_\infty \\
      &= \alpha_{m + 1}; l^{m + 1}_{m + 2}; f^{m + 2}_\infty
    \end{array}

  ここから可換性を以下のように示せる:

  .. math::

    \begin{array}{ll}
      \alpha_\infty; h^\infty_\infty; g^\infty_n
      &= (\bigsqcup_m \alpha_m; l^m_{m + 1}; f^{m + 1}_\infty); h^\infty_\infty; g^\infty_n \\
      &= \bigsqcup_{m > n} \alpha_m; l^m_{m + 1}; f^{m + 1}_\infty; h^\infty_\infty; g^\infty_n \\
      &= \bigsqcup_{m > n} \alpha_m; l^m_{m + 1}; h^{m + 1}_\infty; g^\infty_n \\
      &= \bigsqcup_{m > n} \alpha_m; l^m_{m + 1}; h^{m + 1}_{m + 1}; g^{m + 1}_n \\
      &= \bigsqcup_{m > n} \alpha_m; l^m_{m + 1}; h^{m + 1}_{m + 1}; g^{m + 1}_m; g^m_n \\
      &= \bigsqcup_{m > n} \alpha_m; \mathrm{id}; g^m_n \\
      &= \bigsqcup_{m > n} \alpha_n \\
      &= \alpha_n
    \end{array}

  さて，分解射 :math:`\alpha'_\infty: X \to F^\infty 0` を持ってきた時，

  .. math::

    \begin{array}{ll}
      \alpha_\infty
      &= \bigsqcup_m \alpha_m; l^m_{m + 1}; f^{m + 1}_\infty \\
      &= \bigsqcup_m \alpha'_\infty; h^\infty_\infty; g^\infty_m; l^m_{m + 1}; f^{m + 1}_\infty \\
      &= \bigsqcup_m \alpha'_\infty; h^\infty_\infty; g^\infty_m; l^m_{m + 1}; f^{m + 1}_\infty \\
      &= \alpha'_\infty; (\bigsqcup_m h^\infty_\infty; g^\infty_m; l^m_{m + 1}; f^{m + 1}_\infty)
    \end{array}

  となる．ここで，

  .. math::

    \begin{array}{ll}
      f^n_\infty; (\bigsqcup_m h^\infty_\infty; g^\infty_m; l^m_{m + 1}; f^{m + 1}_\infty)
      &= \bigsqcup_{m > n} f^n_m; h^m_m; l^m_{m + 1}; f^{m + 1}_\infty \\
      &= \bigsqcup_{m > n} f^n_m; f^m_{m + 1}; f^{m + 1}_\infty \\
      &= \bigsqcup_{m > n} f^n_\infty \\
      &= f^n_\infty
    \end{array}

  より， colimit の普遍射の一意性から :math:`\bigsqcup_m h^\infty_\infty; g^\infty_m; l^m_{m + 1}; f^{m + 1}_\infty = \mathrm{id}` ．よって， :math:`\alpha'_\infty = \alpha_\infty` より普遍射の一意性が示せる．

命題.
  CPO enriched な圏 :math:`C` ， order enriched な関手 :math:`F: C \to C` について， :math:`T 1 \xrightarrow{g^1_0} 1 \xrightarrow{l} F 0 \xrightarrow{h^1_1} F 1 \sqsubseteq \mathrm{id}` を満たす :math:`l: 1 \to F 0` が与えられた時， :math:`F^\infty 0 \sim F^\infty 1`

  証明:

  :math:`l^n_{n + 1} = F^n l` とした時，それが上の補題の条件を満たすことを，数学的帰納法で確認する．

  * :math:`n = 0` の時，一意性より :math:`f^0_{n + 1} = \bot` ， :math:`\mathrm{id}: 1 \to 1 = \bot` より成り立つ．
  * :math:`n = m` の時成り立つと仮定すると， :math:`n = m + 1` の時，

    * :math:`h^n_n; l^n_{n + 1} = F h^m_m; F l^m_{m + 1} = F (h^m_m; l^m_{m + 1}) = F f^m_{m + 1} = f^n_{n + 1}`
    * :math:`g^{n + 1}_n; l^n_{n + 1}; h^{n + 1}_{n + 1} = F (g^{m + 1}_m; l^m_{m + 1}; h^{m + 1}_{m + 1}) \sqsubseteq F\, \mathrm{id} = \mathrm{id}`
    * :math:`l^n_{n + 1}; h^{n + 1}_{n + 1}; g^{n + 1}_n = F (l^m_{m + 1}; h^{m + 1}_{m + 1}; g^{m + 1}_m) = F\, \mathrm{id} = \mathrm{id}`

    より成り立つ．

定理.
  CPO enriched な圏 :math:`C` ， order enriched な関手 :math:`F: C \to C` について， :math:`T 1 \xrightarrow{g^1_0} 1 \xrightarrow{l} F 0 \xrightarrow{h^1_1} F 1 \sqsubseteq \mathrm{id}` を満たす :math:`l: 1 \to F 0` が与えられる時， :math:`F` は条件付き algebraically compact ．

  証明:

  上の命題から :math:`F^\infty 0 \sim F^\infty 1` より．

つまり， CPO enriched な状況で， terminal sequence から initial sequence への対応を， :math:`\mathrm{id}` すれすれにいい感じに作れれば良いという感じ．ところで，この対応は pointed CPO の場合 bottom を持ってくることで作れる．

定理.
  pointed CPO enriched な圏 :math:`C` ， order enriched な関手 :math:`F: C \to C` について， :math:`F` は条件付き algebraically compact ．

  証明:

  :math:`l: 1 \to F 0 = \bot` で持ってきた時， :math:`T 1 \xrightarrow{g^1_0} 1 \xrightarrow{l} F 0 \xrightarrow{h^1_1} F 1 = \bot \sqsubseteq \mathrm{id}` より．

さて，今圏は initial / terminal object を持ち， chain cocomplete / cochain complete としているが，空でない pointed CPO enriched な圏においては， chain cocomplete であれば null object (initial でも terminal でもある object) の存在を示せる．

命題.
  空でない pointed CPO enriched な圏 :math:`C` において， chain cocomplete なら null object が存在する．

  証明:

  空でないため圏から object :math:`A \in |C|` を適当に一つ持ってこれる．この時，以下の chain が作れる:

  .. math::

    A \xrightarrow{\bot} A \xrightarrow{\bot} \cdots

  この colimit :math:`A_\infty` を考える．この構成射は :math:`A \xrightarrow{u} A_\infty = A \xrightarrow{\bot} A \xrightarrow{u'} A_\infty = A \xrightarrow{\bot} A_\infty` より， :math:`\bot: A \to A_\infty` になる．任意の :math:`X \in |C|` について， :math:`A \xrightarrow{\bot} X` は cocone になる．この時， colimit からの普遍射 :math:`\alpha: A_\infty \to X` が存在する．また， :math:`\alpha': A_\infty \to X` が存在した時， :math:`A \xrightarrow{\bot} A_\infty \xrightarrow{\alpha'} X = A \xrightarrow{\bot} X` より分解射になる．この時， colimit の普遍性より :math:`\alpha = \alpha'` である．よって， :math:`A_\infty` は initial object になる．

  また， :math:`X \xrightarrow{\beta} A_\infty` があった時， :math:`X \xrightarrow{\beta} A_\infty = X \xrightarrow{\beta} A_\infty \xrightarrow{\mathrm{id} = \bot} A_\infty = X \xrightarrow{\bot} A_\infty` より，任意の :math:`X \in |C|` について :math:`\beta: X \to A_\infty` も一意に存在する．よって， :math:`A_\infty` は terminal object にもなる．

よって，空でない pointed CPO enriched な圏であれば， chain cocomplete を仮定するだけで良い．ところで，ここまでは条件付き algebraically compact ，つまり fixed object を持つ関手のみを対象にしてきたが，関手が CPO enriched ，つまり sup も保存するならば algebraically compact であることが言える．

定理.
  pointed CPO enriched な圏 :math:`C` ， CPO enriched な関手 :math:`F: C \to C` について， :math:`F` は algebraically compact ．

  証明:

  :math:`l: 1 \to F 0 = \bot` で持ってきた時， :math:`F^\infty 0 \sim F^\infty 1` より，後は :math:`F (F^\infty 0) \sim F^\infty 0` を示せばよい．で， :math:`F (F^\infty 0)` が initial sequence の colimit であることを示せば， colimit の一意性から :math:`F (F^\infty 0) \sim F^\infty 0` が言える．なので， colimit であることを示す．

  構成射を

  .. math::

    f'^n_\infty = \begin{array}{ll}
      0 \xrightarrow{!} F (F^\infty 0) &(n = 0) \\
      F f^m_\infty &(n = m + 1)
    \end{array}

  で作る． cocone :math:`(\alpha_n: F^n 0 \to X)_n` に対して， :math:`\alpha_\infty = \bigsqcup_m F h^\infty_\infty; F g^\infty_m; l^{m + 1}_{m + 2}; \alpha_{m + 2}` が普遍射になることを示す．

  :math:`n = 0` の時， initial object の普遍性より :math:`f'^0_\infty; \alpha_\infty = \alpha_0` になることは良い． :math:`n > 0` の時，

  .. math::

    \begin{array}{ll}
      f'^n_\infty; \alpha_\infty
      &= F f^{n - 1}_\infty; (\bigsqcup_m F h^\infty_\infty; F g^\infty_m; l^{m + 1}_{m + 2}; \alpha_{m + 2}) \\
      &= \bigsqcup_m F f^{n - 1}_\infty; F h^\infty_\infty; F g^\infty_m; l^{m + 1}_{m + 2}; \alpha_{m + 2} \\
      &= \bigsqcup_{m > n} F f^{n - 1}_m; F h^m_m; F l^m_{m + 1}; \alpha_{m + 2} \\
      &= \bigsqcup_{m > n} F (f^{n - 1}_m; h^m_m; l^m_{m + 1}); \alpha_{m + 2} \\
      &= \bigsqcup_{m > n} F f^{n - 1}_{m + 1}; \alpha_{m + 2} \\
      &= \bigsqcup_{m > n} f^n_{m + 2}; \alpha_{m + 2} \\
      &= \bigsqcup_{m > n} \alpha_n \\
      &= \alpha_n
    \end{array}

  より，可換になることが示せる．また，分解射 :math:`\alpha'_\infty: F^\infty 0 \to X` について，

  .. math::

    \begin{array}{lll}
      \alpha_\infty
      &= \bigsqcup_m F h^\infty_\infty; F g^\infty_m; l^{m + 1}_{m + 2}; \alpha_{m + 2} \\
      &= \bigsqcup_m F h^\infty_\infty; F g^\infty_m; l^{m + 1}_{m + 2}; f'^{m + 2}_\infty; \alpha'_\infty \\
      &= (\bigsqcup_m F h^\infty_\infty; F g^\infty_m; F l^m_{m + 1}; F f^{m + 1}_\infty); \alpha'_\infty \\
      &= \bigsqcup_m F (h^\infty_\infty; g^\infty_m; l^m_{m + 1}; f^{m + 1}_\infty); \alpha'_\infty \\
      &= F (\bigsqcup_m h^\infty_\infty; g^\infty_m; l^m_{m + 1}; f^{m + 1}_\infty); \alpha'_\infty \\
      &= F\,\mathrm{id}; \alpha'_\infty &(\because \text{colimit の普遍性より}) \\
      &= \alpha'_\infty
    \end{array}

  よって，分解射は一意になる．

なお，例えば pointed CPO による圏自体は， pointed CPO enriched であり， chain cocomplete なので今回の圏の条件を満たしている．よって，これ上の関手が fixed object を持って continuous function の順序を保存するか， continuous function space の sup を保存すれば， algebraically compact になる．

まとめ
------

とりあえず，関手が algebraically compact ，つまり initial algebra と terminal algebra が iso になるには，

* :math:`F^\infty 0 \sim F^\infty 1` であること
* なんらかの algebra と coalgebra の間に relational morphism が作れること

が重要で， relational morphism の方は fixed object があれば作れるので，重要なのは initial sequence の colimit と terminal sequence の limit が一致するかということになる．

さらに， pointed CPO enriched な場合は，関手が order を保存すれば :math:`\bot` からいい感じに :math:`F^\infty 0 \sim F^\infty 1` に繋げるような terminal sequence から initial sequence への射の列が作れる．なので， order を保存するぐらいで algebraically compact になる．

なるほどなという感じ (こなみ) ．

FIXME
  と気持ちよく終わりたかったが，大事な定理が示せねえ... 誰か助けてくれ．

.. [#fleyd-1991] https://link.springer.com/chapter/10.1007/BFb0084215
.. [#fixme-proof] この証明が出来ない．原論文の Theorem 1.7 がそれ．Barr 先生は， "Since an object has only a set of endomorphisms" と言ってるが，この文章が理解できなかった...
