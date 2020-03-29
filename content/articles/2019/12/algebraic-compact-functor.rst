始代数と終余代数が一致する条件
==============================

:tags: 圏論, cpo, F-algebra
:category: 数学

http://ziphil.com/diary/application/26.html を読んでるとき，まとめたことを記事として残しておこうと思った．

参考文献は，

  Barr, M. (1992). Algebraically compact functors. Journal of Pure and Applied Algebra, 82(3), 211–231. https://doi.org/10.1016/0022-4049(92)90169-G

なお以降考える圏は，特に断りのない限り， initial object 0 及び terminal object 1 が存在し，chain cocomplete (chain に対する図式が colimit を持つ) で cochain complete (cochain に対する図式が limit を持つ) であるとする．

F-algebra と F-coalgebra
------------------------

圏 :math:`C`，自己関手 :math:`F: C \to C` において，:math:`F`-algebra は，射 :math:`F X \xrightarrow{a} X` のこと．で，:math:`F`-algebra 同士の射を以下の図式を可換にする射 :math:`h` (:math:`F`-algebra 準同型射と呼ぶ)

.. image:: {attach}algebraic-compact-functor/f-algebra-homomorphism.png
  :alt: :math:`F X_1 \xrightarrow{a_1} X_1`，:math:`F X_2 \xrightarrow{a_2} X_2` に対して，:math:`f \circ a_1 = a_2 \circ F f` を満たす :math:`X_1 \xrightarrow{h} X_2`
  :align: center

とすると， :math:`F`-algebra による圏 :math:`\mathrm{Alg}_C(F)` を構成できる．この圏で initial object が存在したとき，それを initial :math:`F`-algebra と呼ぶ．その双対をそれぞれ :math:`F`-coalgebra ， :math:`\mathrm{CoAlg}_C(F)` ，terminal :math:`F`-coalgebra と呼ぶ．

ところで， initial algebra :math:`F A \xrightarrow{a} A` において， :math:`F A \sim A` であることが知られている．

定理. (Lambek's Theorem)
  自己関手 :math:`F: C \to C` において，initial :math:`F`-algebra :math:`F A \xrightarrow{a} A` があるとする．この時，:math:`a` は同型射．

  証明:

  以下の図式は可換:

  .. image:: {attach}algebraic-compact-functor/initial-f-algebra-iso.png
    :alt: initial algebra に :math:`F` を適用したものを介した， initial algebra から initial algebra への合成射
    :align: center

  よって， initial algebra からの射の一意性から，:math:`a^{-1}; a = \mathrm{id}`．さらに，左の図式だけ考えると :math:`a; a^{-1} = F a^{-1}; F a = F (a^{-1}; a) = F\, \mathrm{id} = \mathrm{id}` より，:math:`a` は同型射になる．

双対的に， terminal :math:`F`-coalgebra についても同型であることが示せる．ところで， initial / terminal object から initial algebra / terminal coalgebra を構成できる場合がある．それは，initial / terminal sequence が途中で同型の列になる場合である．

定義. (initial / terminal :math:`F`-sequence)
  以下の順序数で添字づけられた図式を (ordinal) initial :math:`F`-sequence と呼ぶ．

  .. math::

    0 \xrightarrow{\exists ! f} F 0 \xrightarrow{F f} F^2 0 \xrightarrow{F^2 f} \cdots F^\omega 0 \xrightarrow{F^\omega f} \cdots F^\alpha 0 \xrightarrow{F^\alpha f} \cdots

  なお，任意の順序数 :math:`\alpha, \beta \leq \alpha` について，:math:`F^\alpha 0`，:math:`f^\beta_\alpha: F^\beta 0 \to F^\alpha 0` を，以下のように定める:

  :math:`\alpha = 0` の時
    :math:`F^0 0 = 0`，:math:`f^0_0 = \mathrm{id}`

  :math:`\alpha = 1` の時
    :math:`F^1 0 = F 0`，

    .. math::

      f^\beta_1 = \left\{\begin{array}{ll}
        f &(\beta = 0) \\
        \mathrm{id} &(\beta = \alpha = 1)
      \end{array}\right.

  :math:`\alpha` が極限順序数の時
    :math:`(f^\beta_\alpha: F^\beta 0 \to F^\alpha 0)_{\beta < \alpha}` は chain :math:`(F^\beta 0)_{\beta < \alpha}` の colimit で定める．また，:math:`f^\alpha_\alpha = \mathrm{id}`

  :math:`\alpha = \gamma + 1` で，:math:`\gamma` が極限順序数の時
    :math:`F^\alpha 0 = F (F^\gamma 0)` とする．この時，:math:`\beta < \gamma` に対して，:math:`f^\beta_\alpha = f^\beta_{\beta + 1}; F f^\beta_\gamma` とおくと，:math:`(f^\beta_\alpha: F^\beta 0 \to F^\alpha)_{\beta < \gamma}` は cocone になるので一意射 :math:`f^\gamma_\alpha` が存在する．これらで :math:`\beta \leq \gamma` については :math:`f^\beta_\alpha` を定める．また，:math:`f^\alpha_\alpha = \mathrm{id}`

  :math:`\alpha = \gamma + 2` と書ける時 (つまり，上記以外の場合)
    :math:`F^\alpha = F (F^{\gamma + 1} 0)`，

    .. math::

      f^\beta_\alpha = \left\{\begin{array}{ll}
        f^\beta_{\gamma + 1}; F f^\gamma_{\gamma + 1} &(\beta \leq \gamma + 1) \\
        \mathrm{id} &(\beta = \alpha)
      \end{array}\right.

  この定義が well-defined であること，以下が成り立つことは帰納法により容易に示せる:

  * :math:`F^{\alpha + 1} 0 = F (F^\alpha 0)`
  * :math:`f^{\beta + 1}_{\alpha + 1} = F f^\beta_\alpha`
  * :math:`f^\alpha_\alpha = \mathrm{id}`
  * :math:`\beta \leq \gamma \leq \alpha` の時，:math:`f^\beta_\gamma; f^\gamma_\alpha = f^\beta_\alpha`

  また，以下の順序数で添字づけられた反変図式を (ordinal) terminal :math:`F`-sequence と呼ぶ．

  .. math::

    1 \xleftarrow{\exists ! g} F 1 \xleftarrow{F g} F^2 1 \xleftarrow{F^2 g} \cdots F^\omega 1 \xleftarrow{F^\omega g} \cdots F^\alpha 1 \xrightarrow{F^\alpha g} \cdots

  この時，:math:`F^\alpha 1`，:math:`g^\beta_\alpha: F^\beta 1 \to F^\alpha 1` を initial sequence と双対的に定める．

initial / terminal sequence が途中で同型の列になる時，その列は停止するという．

定義. (initial / terminal sequence の停止性)
  :math:`f^\alpha_{\alpha + 1}` が同型になる時，initial sequence は :math:`\alpha` で停止するという．双対的に，:math:`g^{\alpha + 1}_\alpha` が同型になる時，terminal sequence は :math:`\alpha` で停止するという．

なお，関手は同型を保存するので :math:`\alpha` で停止する initial sequence は :math:`\alpha` 以降の射が全て同型射になる．ところで，initial sequence の停止性の条件に，次のものがある．

命題.
  ある :math:`\alpha, \beta < \alpha` で，:math:`f^\beta_\alpha` が同型射ならば，:math:`f^\beta_{\beta + 1}` は同型射

  証明:

  .. math::

    F f^\beta_\alpha = f^{\beta + 1}_{\alpha + 1} = f^{\beta + 1}_\alpha; f^\alpha_{\alpha + 1}

  より，以下の図式が可換:

  .. image:: {attach}algebraic-compact-functor/initial-sequence-terminates.png
    :alt: :math:`F f^\beta_\alpha = f^{\beta + 1}_\alpha; f^\alpha_{\alpha + 1}`，:math:`f^\beta_\alpha = f^\beta_{\beta + 1}; f^{\beta + 1}_\alpha` への同型射の分解
    :align: center

  この時，:math:`f^{\beta + 1}_\alpha` は同型射になり，よって :math:`f^\beta_{\beta + 1}` も同型射になる．

なお，上記の命題で :math:`f^{\beta + 1}_\alpha` が同型射になることは，以下の命題から分かる．

命題.
  ある射 :math:`a: A \to B` が :math:`a; b_1: A \to A = \mathrm{id}` を満たす :math:`b_1: B \to A` と，:math:`b_2; a: B \to B = \mathrm{id}` を満たす :math:`b_2: B \to A` を持つ時，:math:`a` は同型射 [#notice-split-mono-and-split-epi]_

  証明:

  :math:`b_1; a = \mathrm{id}; b_1; a = b_2; a; b_1; a = b_2; \mathrm{id}; a = b_2; a = \mathrm{id}` より，:math:`b_1` は :math:`a` の逆射より．

停止する initial sequence からは，initial algebra を構成できる．

補題. (initial algebra の構成)
  :math:`\alpha` で initial sequence が停止する時，:math:`F^\alpha 0 \xleftarrow{{f^\alpha_{\alpha + 1}}^{-1}} F (F^\alpha 0)` は initial algebra

  証明:

  任意の algebra :math:`F X \xrightarrow{a} X` について，任意の :math:`\beta \leq \alpha` で :math:`h^\beta: F^\beta 0 \to X` を以下のように定義する．

  :math:`\beta = 0` の時
    :math:`h^0: 0 \to X` は initial object の一意射で定める．

  :math:`\beta` が極限順序数の時
    :math:`(h^\gamma)_{\gamma < \beta}` は cocone になるため，一意射 :math:`h^\beta: F^\beta 0 \to X` が存在する．これで定める．

  :math:`\beta = \gamma + 1` と書ける時
    :math:`h^\beta = F h^\gamma; a` で定める．

  この時，:math:`{f^\alpha_{\alpha + 1}}^{-1}; h^\alpha = F h^\alpha; a` は容易に確かめられる．よって，:math:`h^\alpha: F^\alpha 0 \to X` は準同型．また，準同型 :math:`k: F^\alpha 0 \to X` について，任意の :math:`\beta \leq \alpha` について :math:`k^\beta = f^\beta_\alpha; k` とおくと，:math:`k^\beta = h^\beta` となることは以下のように帰納法で示せる．

  :math:`\beta = 0` の時
    initial object の一意性より正しい．

  :math:`\beta` が極限順序数の時
    :math:`(h^\gamma)_{\gamma < \beta} = (k^\gamma)_{\gamma < \beta}` であるため，:math:`k^\beta: F^\beta 0 \to X` はその cocone への分解射になる．よって，colimit :math:`F^\beta 0` の一意性より正しい．

  :math:`\beta = \gamma + 1` と書ける時
    .. image:: {attach}algebraic-compact-functor/initial-algebra-from-initial-sequence.png
      :alt: :math:`k^\beta = f^{\gamma + 1}_\alpha; k = F f^\gamma_\alpha; F k; a`
      :align: center

    より，i.h. から :math:`k^\beta = F (f^\gamma_\alpha; k); a = F h^\gamma; a = h^\beta` より正しい．

  よって，:math:`k = k^\alpha = h^\alpha` より準同型は一意に定まることから，題意は示された．

双対的に，停止する terminal sequence から terminal coalgebra が構成できる．この具体的な設定としては，例えば :math:`F` が colimit を保存すれば良い．

定理. (Adámek's Theorem)
  :math:`F: C \to C` が colimit を保存する時，同型射 :math:`F (F^\omega 0) \sim F^\omega 0` が存在し，initial algebra

  証明:

  :math:`F^\omega 0 \sim \mathop{\mathrm{colim}}_{n < \omega} F^{n + 1} 0 \sim F (F^\omega 0)` より．

なお，今回は ordinal chain で initial sequence を作っているが，上記の定理は countable chain complete ぐらいで成り立つ．双対的に terminal coalgebra も，:math:`F: C \to C` が limit を保存する時構成できる．さて，ここからが本題．

まず， algebra から coalgebra への準同型射を以下のように定義する．

定義. (relational :math:`F`-morphism)
  :math:`F`-algebra :math:`F A \xrightarrow{a} A` 及び :math:`F`-coalgebra :math:`B \xrightarrow{b} F B` について，以下の図式を満たす :math:`m: A \to B` を relational :math:`F`-morphism と呼ぶ:

  .. image:: {attach}algebraic-compact-functor/relational-f-morphism.png
    :alt: :math:`a; m; b = F m`
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

ところで，initial sequence と terminal sequence の間には relational morphism を設定できる．

定義. (relational morphism from initial sequence to terminal sequence)
  :math:`h^\alpha_\alpha: F^\alpha 0 \to F^\alpha 1` を以下のように定義する:

  :math:`\alpha = 0` の時
    :math:`h^0_0: 0 \to 1` は initial object から terminal object への一意射で定める．

  :math:`\alpha` が極限順序数の時
    まず，:math:`\beta < \alpha` を固定したとき，後述する :math:`h^\beta_\gamma: F^\beta 0 \to F^\gamma 1` のようなものが考えられ，この時 :math:`(h^\beta_\gamma)_{\gamma < \alpha}` は cone になり limit :math:`F^\alpha 1` への普遍射 :math:`h^\beta_\alpha: F^\beta 0 \to F^\alpha 1` が作れる．さらに，:math:`(h^\beta_\alpha)_{\beta < \alpha}` は cocone になり colimit :math:`F^\alpha 0` からの普遍射 :math:`h^\alpha_\alpha: F^\alpha 0 \to F^\alpha 1` が作れる．なお，これは作る順序を変えても普遍性より同じ射が作れる．これで定める．

  :math:`\alpha = \gamma + 1` と書ける時
    :math:`h^\alpha_\alpha = F h^\gamma_\gamma` で定める．

  なおこの時， :math:`h^\alpha_\beta: F^\alpha 0 \to F^\beta 1` を以下のように定義する．

  .. math::

    h^\alpha_\beta = \left\{\begin{array}{ll}
      f^\alpha_\beta; h^\beta_\beta &(\alpha < \beta) \\
      h^\alpha_\alpha &(\alpha = \beta) \\
      h^\alpha_\alpha; g^\alpha_\beta &(\alpha > \beta)
    \end{array}\right.

ところで，全ての relational morphism は，initial sequence から terminal sequence への relational morphism に分解できる．

命題.
  algebra :math:`F A \xrightarrow{a} A`，coalgebra :math:`B \xrightarrow{b} F B` について，relational morphism :math:`m: A \to B` が存在する時，initial algebra の構成の補題と同様の作り方で :math:`h^\alpha: F^\alpha 0 \to A` を作成し，双対的に :math:`h_\alpha: B \to F^\alpha 1` を作成した時，:math:`h^\alpha_\alpha = h^\alpha; m; h_\alpha`

  証明:

  :math:`\alpha` に関する帰納法で示す．

  :math:`\alpha = 0` の時
    initial object の一意性から正しい．

  :math:`\alpha` が極限順序数の時
    :math:`h^\alpha_\alpha` の定義と limit，colimit の一意性，i.h. から正しい．

  :math:`\alpha = \gamma + 1` と書ける時
    以下が可換になるので，i.h. から :math:`h^\alpha_\alpha = F h^\gamma_\gamma = F h^\gamma; F m; F h_\gamma = h^\alpha; m; h_\alpha` より正しい．

    .. image:: {attach}algebraic-compact-functor/ordinal-cond-for-relational-from-ini-to-ter.png
      :alt: :math:`h^\gamma; m; h_\gamma = f^\gamma_{\gamma + 1}; F h^\gamma; F m; F h_\gamma; g^{\gamma + 1}_\gamma`
      :align: center

ここまでが準備．

Algebraically Compact
---------------------

initial algebra と terminal coalgebra が一致するような functor を， algebraically compact と呼ぶ．

定義. (algebraically compact functor)
  圏 :math:`C` に対して，自己関手 :math:`F: C \to C` が initial :math:`F`-algebra と terminal :math:`F`-algebra を持ち，canonical isomorphic になる時，:math:`F` は algebraically compact だと呼ぶ．また，:math:`F` が fixed object を持つならば algebraically compact である時，条件付き algebraically compact であると呼ぶ．

ところで， initial algebra や terminal coalgebra は fixed object なので， fixed object がないというのはつまり，関手が initial algebra や terminal coalgebra をそもそも作れる構造を持っていないということになる．つまり，条件付き algebraically compact とは，関手がそもそも initial algebra や terminal coalgebra を持てる構造にある前提で，その一致性があるというものになる．前の系を思い出すと， fixed object があれば relational morphism は作れるので，後重要なのは initial sequence の colimit と terminal sequence の limit が一致するかということになる．なお，自明だが algebraically compact なら条件付き algebraically compact である．

ついでに， category に対してのざっくりとした algebraically compact 性も定められている．

定義. (algebraically compact category)
  圏 :math:`C` に対して，任意の自己関手 :math:`F: C \to C` が algebraically compact である時， :math:`C` を algebraically compact と呼ぶ．また，任意の fixed object を持つ :math:`F: C \to C` が algebraically compact である時，:math:`C` を条件付き algebraically compact であると呼ぶ．

定義. (algebraically complete category)
  圏 :math:`C` に対して，任意の自己関手 :math:`F: C \to C` が initial :math:`F`-algebra を持つ時，:math:`C` を algebraically complete と呼ぶ．

algebraically complete というのは Fleyd が導入した言葉 [#fleyd-1991]_ ．なお，algebraically compact category は algebraically complete category．さて，具体的にどういう条件下だと algebraically compact になるんだろうか？ 1つの条件としては，以下のものがある．

定理. (algebraically compact の十分条件)
  :math:`F: C \to C` について，以下を満たす時 :math:`F` は algebraically compact

  * ある :math:`\alpha_0` が存在して，任意の :math:`\alpha > \alpha_0` で :math:`h^\alpha_\alpha: F^\alpha 0 \to F^\alpha 1` が同型射
  * ある algebra :math:`F A \xrightarrow{a} A` 及び coalgebra :math:`B \xrightarrow{b} F B` の間の relational morphism :math:`A \xrightarrow{m} B` が存在する

  証明:

  この時，任意の :math:`\alpha > \alpha_0` で :math:`h^\alpha; m; h_\alpha = h^\alpha_\alpha` が同型射になる．ここで，:math:`e_\alpha = A \xrightarrow{m} B \xrightarrow{h_\alpha} F^\alpha 1 \xrightarrow{{h^\alpha_\alpha}^{-1}} F^\alpha 0 \xrightarrow{h^\alpha} A` を考えると，

  .. math::

    e_\alpha; e_\alpha = m; h_\alpha; {h^\alpha_\alpha}^{-1}; h^\alpha; m; h_\alpha; {h^\alpha_\alpha}^{-1}; h^\alpha = m; h_\alpha; {h^\alpha_\alpha}^{-1}; h^\alpha_\alpha; {h^\alpha_\alpha}^{-1}; h^\alpha = m; h_\alpha; \mathrm{id}; {h^\alpha_\alpha}^{-1}; h^\alpha = e_\alpha

  を満たす．ところで，:math:`\alpha_0 < \alpha` となる :math:`\alpha` 全体は集合を超えることが知られているので，:math:`\alpha_0 < \alpha` における :math:`e_\alpha` 全体が集合になるためには，ある :math:`\alpha_0 < \alpha_1 < \alpha_2` で :math:`e_{\alpha_1} = e_{\alpha_2}` となる必要がある [#term-have-only-a-set]_ ．この時，

  .. image:: {attach}algebraic-compact-functor/finally-canonical-iso-from-relational-morphism.png
      :alt: :math:`h^{\alpha_2}_{\alpha_2}; g^{\alpha_2}_{\alpha_1}; {h^{\alpha_1}_{\alpha_1}}^{-1}; f^{\alpha_1}_{\alpha_2} = h^{\alpha_2}; e_{\alpha_1}; m; h_{\alpha_2}; {h^{\alpha_2}_{\alpha_2}}^{-1} = h^{\alpha_2}; e_{\alpha_2}; m; h_{\alpha_2}; {h^{\alpha_2}_{\alpha_2}}^{-1} = \mathrm{id}`
      :align: center

  が可換になるので，:math:`(h^{\alpha_2}_{\alpha_2}; g^{\alpha_2}_{\alpha_1}; {h^{\alpha_1}_{\alpha_1}}^{-1}); f^{\alpha_1}_{\alpha_2} = \mathrm{id}` で，逆も :math:`f^{\alpha_1}_{\alpha_2}; (h^{\alpha_2}_{\alpha_2}; g^{\alpha_2}_{\alpha_1}; {h^{\alpha_1}_{\alpha_1}}^{-1}) = \mathrm{id}` が成り立つことが同様に確かめられる．よって，:math:`f^{\alpha_1}_{\alpha_2}` は同型射であり，この時 :math:`f^{\alpha_1}_{\alpha_1 + 1}` も同型射．つまり， initial sequence が停止し，:math:`F^{\alpha_1} 0 \xleftarrow{{f^{\alpha_1}_{\alpha_1 + 1}}^{-1}} F (F^{\alpha_1} 0)` は initial algebra になる．同様に :math:`F^{\alpha_1} 1 \xrightarrow{{g^{\alpha_1 + 1}_{\alpha_1}}^{-1}} F (F^{\alpha_1} 1)` は terminal coalgebra であり，:math:`h^{\alpha_1}_{\alpha_1}` は canonical iso になる．

ところで，ここから条件付き algebraically compact の条件が以下のようになることも分かる．

系. (条件付き algebraically compact の十分条件)
  :math:`F: C \to C` について，以下を満たす時 :math:`F` は algebraically compact

  * ある :math:`\alpha_0` が存在して，任意の :math:`\alpha > \alpha_0` で :math:`h^\alpha_\alpha: F^\alpha 0 \to F^\alpha 1` が同型射

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

  この時， :math:`F^\omega 0 \sim F^\omega 1`

  証明:

  :math:`F^\omega 0` が terminal sequence の limit であることを示せば， limit の一意性から言える．まず，cone :math:`(h_n: X \to F^n 1)_{n \in \mathbb{N}}` を取ってきたとき，この cone から :math:`(h^\omega_\omega; g^\omega_n: F^\omega 0 \to F^n 1)_{n \in \mathbb{N}}` への普遍射が :math:`h_\omega = \bigsqcup_{m \in \mathbb{N}} h_m; l^m_{m + 1}; f^{m + 1}_\omega` であることを示す．

  さて，任意の :math:`n \in \mathbb{N}` に対して :math:`h_\omega = \bigsqcup_{m > n} h_m; l^m_{m + 1}; f^{m + 1}_\omega` ，つまり :math:`(h_m; l^m_{m + 1}; f^{m + 1}_\omega)_{m \in \mathbb{N}}` が単調増加であることを示す．

  .. math::

    \begin{array}{ll}
      h_m; l^m_{m + 1}; f^{m + 1}_\omega
      &= h_{m + 1}; g^{m + 1}_m; l^m_{m + 1}; f^{m + 1}_{m + 2}; f^{m + 2}_\omega \\
      &= h_{m + 1}; g^{m + 1}_m; l^m_{m + 1}; h^{m + 1}_{m + 1}; l^{m + 1}_{m + 2}; f^{m + 2}_\omega \\
      &\sqsubseteq h_{m + 1}; \mathrm{id} ;l^{m + 1}_{m + 2}; f^{m + 2}_\omega \\
      &= h_{m + 1}; l^{m + 1}_{m + 2}; f^{m + 2}_\omega
    \end{array}

  ここから可換性を以下のように示せる:

  .. math::

    \begin{array}{ll}
      h_\omega; h^\omega_\omega; g^\omega_n
      &= (\bigsqcup_{m \in \mathbb{N}} h_m; l^m_{m + 1}; f^{m + 1}_\omega); h^\omega_\omega; g^\omega_n \\
      &= \bigsqcup_{m > n} h_m; l^m_{m + 1}; f^{m + 1}_\omega; h^\omega_\omega; g^\omega_n \\
      &= \bigsqcup_{m > n} h_m; l^m_{m + 1}; h^{m + 1}_\omega; g^\omega_n \\
      &= \bigsqcup_{m > n} h_m; l^m_{m + 1}; h^{m + 1}_{m + 1}; g^{m + 1}_n \\
      &= \bigsqcup_{m > n} h_m; l^m_{m + 1}; h^{m + 1}_{m + 1}; g^{m + 1}_m; g^m_n \\
      &= \bigsqcup_{m > n} h_m; \mathrm{id}; g^m_n \\
      &= \bigsqcup_{m > n} h_n \\
      &= h_n
    \end{array}

  さて，分解射 :math:`h'_\omega: X \to F^\omega 0` を持ってきた時，

  .. math::

    \begin{array}{ll}
      h_\omega
      &= \bigsqcup_{m \in \mathbb{N}} h_m; l^m_{m + 1}; f^{m + 1}_\omega \\
      &= \bigsqcup_{m \in \mathbb{N}} h'_\omega; h^\omega_\omega; g^\omega_m; l^m_{m + 1}; f^{m + 1}_\omega \\
      &= \bigsqcup_{m \in \mathbb{N}} h'_\omega; h^\omega_\omega; g^\omega_m; l^m_{m + 1}; f^{m + 1}_\omega \\
      &= h'_\omega; (\bigsqcup_m h^\omega_\omega; g^\omega_m; l^m_{m + 1}; f^{m + 1}_\omega)
    \end{array}

  となる．ここで，

  .. math::

    \begin{array}{ll}
      f^n_\omega; (\bigsqcup_{m \in \mathbb{N}} h^\omega_\omega; g^\omega_m; l^m_{m + 1}; f^{m + 1}_\omega)
      &= \bigsqcup_{m > n} f^n_m; h^m_m; l^m_{m + 1}; f^{m + 1}_\omega \\
      &= \bigsqcup_{m > n} f^n_m; f^m_{m + 1}; f^{m + 1}_\omega \\
      &= \bigsqcup_{m > n} f^n_\omega \\
      &= f^n_\omega
    \end{array}

  より， colimit の普遍射の一意性から :math:`\bigsqcup_{m \in \mathbb{N}} h^\omega_\omega; g^\omega_m; l^m_{m + 1}; f^{m + 1}_\omega = \mathrm{id}` ．よって， :math:`h'_\omega = h_\omega` より普遍射の一意性が示せる．

命題.
  CPO enriched な圏 :math:`C` ， order enriched な関手 :math:`F: C \to C` について， :math:`T 1 \xrightarrow{g^1_0} 1 \xrightarrow{l} F 0 \xrightarrow{h^1_1} F 1 \sqsubseteq \mathrm{id}` を満たす :math:`l: 1 \to F 0` が与えられた時， :math:`F^\omega 0 \sim F^\omega 1`

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

  上の命題から :math:`F^\omega 0 \sim F^\omega 1` より．

つまり， CPO enriched な状況で， terminal sequence から initial sequence への対応を， :math:`\mathrm{id}` すれすれにいい感じに作れれば良いという感じ．ところで，この対応は pointed CPO の場合 bottom を持ってくることで作れる．

定理.
  pointed CPO enriched な圏 :math:`C`，order enriched な関手 :math:`F: C \to C` について，:math:`F` は条件付き algebraically compact ．

  証明:

  :math:`l: 1 \to F 0 = \bot` で持ってきた時， :math:`T 1 \xrightarrow{g^1_0} 1 \xrightarrow{l} F 0 \xrightarrow{h^1_1} F 1 = \bot \sqsubseteq \mathrm{id}` より．

さて，今圏は initial / terminal object を持ち， chain cocomplete / cochain complete としているが，空でない pointed CPO enriched な圏においては， chain cocomplete であれば null object (initial でも terminal でもある object) の存在を示せる．

命題.
  空でない pointed CPO enriched な圏 :math:`C` において，chain cocomplete なら null object が存在する．

  証明:

  空でないため圏から object :math:`A \in |C|` を適当に一つ持ってこれる．この時，以下の chain が作れる:

  .. math::

    A \xrightarrow{\bot} A \xrightarrow{\bot} \cdots

  この colimit :math:`A_\infty` を考える．この構成射は :math:`A \xrightarrow{u} A_\infty = A \xrightarrow{\bot} A \xrightarrow{u'} A_\infty = A \xrightarrow{\bot} A_\infty` より，:math:`\bot: A \to A_\infty` になる．任意の :math:`X \in |C|` について， :math:`A \xrightarrow{\bot} X` は cocone になる．この時，colimit からの普遍射 :math:`!: A_\infty \to X` が存在する．また， :math:`!': A_\infty \to X` が存在した時， :math:`A \xrightarrow{\bot} A_\infty \xrightarrow{!'} X = A \xrightarrow{\bot} X` より分解射になる．この時，colimit の普遍性より :math:`! = !'` である．よって， :math:`A_\infty` は initial object になる．

  また， :math:`X \xrightarrow{!} A_\infty` があった時， :math:`X \xrightarrow{!} A_\infty = X \xrightarrow{!} A_\infty \xrightarrow{\mathrm{id} = \bot} A_\infty = X \xrightarrow{\bot} A_\infty` より，任意の :math:`X \in |C|` について :math:`!: X \to A_\infty` も一意に存在する．よって， :math:`A_\infty` は terminal object にもなる．

よって，空でない pointed CPO enriched な圏であれば， chain cocomplete を仮定するだけで良い．ところで，ここまでは条件付き algebraically compact ，つまり fixed object を持つ関手のみを対象にしてきたが，関手が CPO enriched ，つまり sup も保存するならば algebraically compact であることが言える．

定理.
  pointed CPO enriched な圏 :math:`C`，CPO enriched な関手 :math:`F: C \to C` について，:math:`F` は algebraically compact ．

  証明:

  :math:`l: 1 \to F 0 = \bot` で持ってきた時，:math:`F^\omega 0 \sim F^\omega 1`．ところで，実はこの時 :math:`F^\omega 0 \sim F (F^\omega 0)` が示せる．これが成り立てば，:math:`F^\omega 0` は fixed object になるので，題意が言える．よって，これを示す．これは，:math:`F (F^\omega 0)` が

  .. math::

    0 \xrightarrow{f^0_1} F 0 \xrightarrow{f^1_2} \cdots F^n 0 \xrightarrow{f^n_{n + 1}} \cdots

  の colimit であることを示せれば，colimit の一意性から言える．

  構成射を

  .. math::

    f'^n_\omega: F^n 0 \to F (F^\omega 0) = \left\{\begin{array}{ll}
      0 \xrightarrow{!} F (F^\omega 0) &(n = 0) \\
      F f^m_\omega &(n = m + 1)
    \end{array}\right.

  で作る．cocone :math:`(h_n: F^n 0 \to X)_{n \in \mathbb{N}}` に対して，:math:`h_\omega = \bigsqcup_{m \in \mathbb{N}} F h^\omega_\omega; F g^\omega_m; l^{m + 1}_{m + 2}; h_{m + 2}` が普遍射になることを示す．

  :math:`n = 0` の時， initial object の普遍性より :math:`f'^0_\omega; h_\omega = h_0` になることは良い．:math:`n > 0` の時，

  .. math::

    \begin{array}{ll}
      f'^n_\omega; h_\omega
      &= F f^{n - 1}_\omega; (\bigsqcup_{m \in \mathbb{N}} F h^\omega_\omega; F g^\omega_m; l^{m + 1}_{m + 2}; h_{m + 2}) \\
      &= \bigsqcup_{m \in \mathbb{N}} F f^{n - 1}_\omega; F h^\omega_\omega; F g^\omega_m; l^{m + 1}_{m + 2}; h_{m + 2} \\
      &= \bigsqcup_{m > n} F f^{n - 1}_m; F h^m_m; F l^m_{m + 1}; h_{m + 2} \\
      &= \bigsqcup_{m > n} F (f^{n - 1}_m; h^m_m; l^m_{m + 1}); h_{m + 2} \\
      &= \bigsqcup_{m > n} F f^{n - 1}_{m + 1}; h_{m + 2} \\
      &= \bigsqcup_{m > n} f^n_{m + 2}; h_{m + 2} \\
      &= \bigsqcup_{m > n} h_n \\
      &= h_n
    \end{array}

  より，可換になることが示せる．また，分解射 :math:`h'_\omega: F (F^\omega 0) \to X` について，

  .. math::

    \begin{array}{lll}
      h_\omega
      &= \bigsqcup_{m \in \mathbb{N}} F h^\omega_\omega; F g^\omega_m; l^{m + 1}_{m + 2}; h_{m + 2} \\
      &= \bigsqcup_{m \in \mathbb{N}} F h^\omega_\omega; F g^\omega_m; l^{m + 1}_{m + 2}; f'^{m + 2}_\omega; h'_\omega \\
      &= (\bigsqcup_{m \in \mathbb{N}} F h^\omega_\omega; F g^\omega_m; F l^m_{m + 1}; F f^{m + 1}_\omega); h'_\omega \\
      &= \bigsqcup_{m \in \mathbb{N}} F (h^\omega_\omega; g^\omega_m; l^m_{m + 1}; f^{m + 1}_\omega); h'_\omega \\
      &= F (\bigsqcup_{m \in \mathbb{N}} h^\omega_\omega; g^\omega_m; l^m_{m + 1}; f^{m + 1}_\omega); h'_\omega \\
      &= F\,\mathrm{id}; h'_\omega &(\because \text{colimit の普遍性より}) \\
      &= h'_\omega
    \end{array}

  よって，分解射は一意になる．

なお，例えば pointed CPO による圏自体は，pointed CPO enriched であり，chain cocomplete なので今回の圏の条件を満たしている．よって，これ上の関手が fixed object を持って continuous function の順序を保存するか，continuous function space の sup を保存すれば，algebraically compact になる．

まとめ
------

とりあえず，関手が algebraically compact，つまり initial algebra と terminal algebra が iso になるには，

* どこかの :math:`\alpha` で :math:`F^\alpha 0 \sim F^\alpha 1` になること
* なんらかの algebra と coalgebra の間に relational morphism が作れること

が重要で，relational morphism の方は fixed object があれば作れるので，重要なのは initial sequence と terminal sequence がどこかで iso になるかということになる．

さらに， pointed CPO enriched な場合は，関手が order を保存すれば :math:`\bot` からいい感じに :math:`F^\omega 0 \sim F^\omega 1` に繋げるような terminal sequence から initial sequence への射の列が作れる．なので， order を保存するぐらいで algebraically compact になる．

なるほどなという感じ (こなみ)．

.. [#fleyd-1991] https://link.springer.com/chapter/10.1007/BFb0084215
.. [#notice-split-mono-and-split-epi] なお，:math:`b_1` を持つことを :math:`a` は split monomorphism である，:math:`b_2` を持つことを :math:`a` は split epimorphism であるという．この命題はもう少し条件を弱め，:math:`a` が mono かつ split epi，または epi かつ split mono でも成立する．
.. [#term-have-only-a-set] これを， *an object has only a set of endomorphisms* と元論文で言ってる．最初のこの文の意味が分からなくって， https://twitter.com/Ziphil/status/1206575467672690688 で教えてもらった． Ziphil さん，ありがとうございました．
