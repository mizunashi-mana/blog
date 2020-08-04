ハイティング代数の性質
======================

:tags: ハイティング代数, ブール代数, 束
:category: 数学

古典論理の意味論がブール代数によって議論できるように，直観主義論理の意味論はハイティング代数によって考えることができる．ハイティング代数とは，冪で閉じている有界束であり，ブール代数もハイティング代数になる．

というわけで結構題材になるハイティング代数だが，巷でハイティング代数に関しての資料が少なかったりするので，自分のために諸々備忘録を残しておく．

ハイティング代数の定義
----------------------

まずは基本的なところから。最初に poset を導入する。

定義. 半順序 (partial order, poset)
  集合 :math:`A` とそれ上の二項関係 :math:`(\sqsubseteq) \subseteq A \times A` の組 :math:`(A, \sqsubseteq)` で以下を満たすものを、半順序と呼ぶ:

  反射律 (reflexivity)
    任意の :math:`x \in A` について、:math:`x \sqsubseteq x`

  推移律 (transitivity)
    任意の :math:`x, y, z \in A` について、:math:`x \sqsubseteq y` かつ :math:`y \sqsubseteq z` ならば、:math:`x \sqsubseteq z`

  反対称律 (anti-symmetric)
    任意の :math:`x, y \in A` について、:math:`x \sqsubseteq y` かつ :math:`y \sqsubseteq x` ならば、:math:`x = y`

束の定義方法は色々あるが、今回は poset ベースの束の定義を導入する。

定義. 束 (lattice)
  半順序 :math:`(A, \sqsubseteq)` で以下を満たすものを、束と呼ぶ:

  * 任意の :math:`x, y \in A` について、上限 :math:`x \lor y \in A` が存在する。この時、:math:`x \lor y` を結び (join) と呼ぶ
  * 任意の :math:`x, y \in A` について、下限 :math:`x \land y \in A` が存在する。この時、:math:`x \land y` を交わり (meet) と呼ぶ

束は、幾つかの代数的性質が成り立つ。

定理. 束の性質
  束 :math:`(L, \sqsubseteq)` について、以下が成り立つ:

  交換律 (commutative)
    任意の :math:`x, y \in L` について、:math:`x \land y = y \land x`、:math:`x \lor y = y \lor x`

  結合律 (associative)
    任意の :math:`x, y, z \in L` について、:math:`x \land (y \land z) = (x \land y) \land z`、:math:`x \lor (y \lor z) = (x \lor y) \lor z`

  吸収律 (absorption)
    任意の :math:`x, y \in L` について、:math:`x \land (x \lor y) = x \lor (x \land y) = x`

  冪等律 (idempotent)
    任意の :math:`x \in L` について、:math:`x \land x = x \lor x = x`

  証明:

  交換律，結合律，冪等律はいいと思うので，吸収律だけ示しておく．といっても，こいつもほぼ自明で，:math:`x \leq x \lor y`，:math:`x \land y \leq x` より，下限，上限の定義から :math:`x \land (x \lor y) = x`，:math:`x \lor (x \land y) = x`．

束の中で、空集合に対する上限、下限が存在するもの、つまり最大元、最小元が存在するものを有界束と呼ぶ。

定義. 有界束 (bounded lattice)
  束 :math:`(L, \sqsubseteq)` で以下を満たすものを，有界束と呼ぶ:

  * ある :math:`\top \in L` が存在し、任意の :math:`x \in L` について :math:`x \leq \top` を満たす
  * ある :math:`\bot \in L` が存在し、任意の :math:`x \in L` について :math:`\bot \leq x` を満たす

ハイティング代数は、有界束の中で冪対象を持つものである。

定義. ハイティング代数 (heyting algebra)
  有界束 :math:`(L, \sqsubseteq)` で以下を満たすものを、ハイティング代数と呼ぶ:

  * 任意の :math:`x, y \in L` について、:math:`\{z \in L \mid x \land z \leq y\}` の上限 :math:`x \to y \in L` が存在する。この時、:math:`x \to y` を冪 (exponential) または相対擬補元 (relative pseudo-complement) と呼ぶ

証明は省略するが，冪の例は :math:`x \to x = \top`，:math:`\top \to x = x`，:math:`\bot \to x = \top` などがある．

ハイティング代数の性質
----------------------

分配律を満たす束を分配束という．

定義. 分配束 (distributive lattice)
  束 :math:`(L, \sqsubseteq)` について、以下を満たすものを分配束と呼ぶ:

  * 任意の :math:`x, y, z \in L` について、:math:`x \land (y \lor z) = (x \land y) \lor (x \land z)`
  * 任意の :math:`x, y, z \in L` について、:math:`x \lor (y \land z) = (x \lor y) \land (x \lor z)`

なお，片方のみ成り立てば分配束になる．

定理. 分配束の条件
  束 :math:`(L, \sqsubseteq)` について、以下は同値．

  1. :math:`(L, \sqsubseteq)` は分配束
  2. 任意の :math:`x, y, z \in L` について、:math:`x \land (y \lor z) = (x \land y) \lor (x \land z)`
  3. 任意の :math:`x, y, z \in L` について、:math:`x \lor (y \land z) = (x \lor y) \land (x \lor z)`

  証明:

  1 ならば 2，1 ならば 3 は自明．2 ならば 1 は，

  .. math::

    (x \lor y) \land (x \lor z)
    = ((x \lor y) \land x) \lor ((x \lor y) \land z)
    = x \lor (x \land z) \lor (y \land z)
    = x \lor (y \land z)

  より，示せる．3 ならば 1 も対称的に証明できる．

ハイティング代数は、分配束になる．

定理. ハイティング代数の分配律
  ハイティング代数は，分配束である．

  証明:

  * :math:`x \land y \leq x \land (y \lor z)`
  * :math:`x \land z \leq x \land (y \lor z)`

  より，:math:`x \land (y \lor z)` は :math:`x \land y`，:math:`x \land z` の上界になる．よって，:math:`(x \land y) \lor (x \land z) \leq x \land (y \lor z)`．また，

  * :math:`x \land y \leq (x \land y) \lor (x \land z)`
  * :math:`x \land z \leq (x \land y) \lor (x \land z)`

  より，

  * :math:`y \leq (x \to (x \land y) \lor (x \land z))`
  * :math:`z \leq (x \to (x \land y) \lor (x \land z))`

  よって，:math:`x \to (x \land y) \lor (x \land z)` は :math:`y`，:math:`z` の上界より :math:`y \lor z \leq (x \to (x \land y) \lor (x \land z))`．ここから，

  .. math::

    x \land (y \lor z) \leq x \land (x \to (x \land y) \lor (x \land z)) \leq (x \land y) \lor (x \land z)

  よって，:math:`x \land (y \lor z) = (x \land y) \lor (x \land z)` より，分配束になる．

ついでにだが、通常の有界束は分配律が成り立つとは限らない。反例は以下のもの:

.. image:: {attach}heyting-algebra-properties/not-distributive-lattice.png
  :alt: :math:`\bot < x, y, z < \top`
  :align: center

この時、:math:`x \lor (y \land z) = x \neq \top = (x \lor y) \land (x \lor z)` になる。さて，束の元には補元が存在する場合がある．

定義. 補 (complement)
  有界束 :math:`(L, \sqsubseteq)` の元 :math:`x \in L` について，

  * :math:`x \land y \leq \bot`
  * :math:`\top \leq x \lor y`

  を満たす :math:`y \in L` を :math:`x` の補と呼ぶ

:math:`\top` と :math:`\bot` は互いに補の関係になる．なお，有界束において一般に補は一意とは限らない．例えば，上の分配律が成り立たない束において，:math:`x` の補は :math:`y`，:math:`z` 両方になる．もちろん存在するとも限らない．これは，線形な束を思い描いてみるといいだろう．ところで，分配束において補は存在するなら一意に定まる．

定理. 分配束における補の一意性
  有界な分配束 :math:`(L, \sqsubseteq)` について，

  * 任意の :math:`x, y, z \in L` について、:math:`x \land (y \lor z) = (x \land y) \lor (x \land z)`
  * 任意の :math:`x, y, z \in L` について、:math:`x \lor (y \land z) = (x \lor y) \land (x \lor z)`

  が成り立つ時，任意の :math:`x \in L` についてその補は高々1つである

  証明:

  :math:`x \in L` の補 :math:`y_1 \in L`，:math:`y_2 \in L` が存在する時，

  .. math::

    y_1 = y_1 \land (y_2 \lor x) = (y_1 \land y_2) \lor (y_1 \land x) = (y_1 \land y_2) \lor (y_2 \land x) = y_2 \land (y_1 \lor x) = y_2

  より，:math:`y_1 = y_2`．よって，存在すれば補は一意．

ハイティング代数において補は存在するとは限らないが，ハイティング代数は分配束より存在すれば一意である．さらに，ハイティング代数においては補が存在すればその形を特定できる．

定義. 擬似補 (pseudo-complement)
  ハイティング代数 :math:`(H, \sqsubseteq)` について、その要素 :math:`x \in H` に対し、:math:`x \to \bot` を擬似補と言い、:math:`\neg x` と表記する

ハイティング代数において，補が存在すればそれは :math:`\neg x` と一致する．

定理. 擬似補と補の一意性
  ハイティング代数 :math:`(H, \sqsubseteq)` の元 :math:`x \in H` について，その補が存在する時，それは :math:`\neg x` のみである

  証明:

  :math:`x \in H` の補 :math:`y \in H` が存在する時，:math:`x \land y \leq \bot` より :math:`y \leq \neg x` である．この時，

  * :math:`x \land \neg x \leq \bot`
  * :math:`\top \leq x \lor y \leq x \lor \neg x`

  より，:math:`\neg x` は補である．さらに，ハイティング代数は分配束なので，補は :math:`\neg x` のみである．

ハイティング代数ではド・モルガン則は片方のみ成り立つ．

定理. ド・モルガン則 (De Morgan's law)
  ハイティング代数 :math:`(H, \sqsubseteq)` について、以下が成り立つ:

  * 任意の :math:`x, y \in H` について、:math:`\neg x \land \neg y = \neg (x \lor y)`

  証明:

  .. math::

    (x \lor y) \land \neg x \land \neg y
    = (x \land \neg x \land \neg y) \lor (y \land \neg \land \neg y)
    = \bot \lor \bot
    \leq \bot

  より，:math:`\neg x \land \neg y \leq \neg (x \lor y)`．また，

  .. math::

    x \land \neg (x \lor y)
    \leq (x \land \neg (x \lor y)) \lor (y \land \neg (x \lor y))
    = (x \lor y) \land \neg (x \lor y)
    \leq \bot

  より，:math:`\neg (x \lor y) \leq \neg x`．対称性より :math:`\neg (x \lor y) \leq \neg y`．よって，:math:`\neg (x \lor y) \leq \neg x \land \neg y` より，:math:`\neg x \land \neg y = \neg (x \lor y)`．

ブール代数と二重否定変換
------------------------

ハイティング代数のインスタンスにブール代数がある．

定義. ブール代数 (boolean algebra)
  分配束 :math:`(L, \sqsubseteq)` で以下を満たすものを、ブール代数と呼ぶ:

  * 任意の :math:`x \in L` について、その補が存在する

定理. ブール代数はハイティング代数
  ブール代数 :math:`(B, \sqsubseteq)` は，ハイティング代数である

  証明:

  :math:`x, y \in B` について，:math:`x` の補を :math:`a` とする．この時，:math:`x \to y = a \lor y` を示す．

  .. math::

    x \land (a \lor y) = (x \land a) \lor (x \land y) = x \land y \leq y

  であり，:math:`x \land z \leq y` となる :math:`z \in B` について，

  .. math::

    z = z \land (a \lor x) = (z \land a) \lor (x \land z) \leq a \lor y

  より，:math:`a \lor y` は :math:`x \to y` の条件を満たす．よって，任意の元について冪が存在するため，:math:`(B, \sqsubseteq)` はハイティング代数．

ハイティング代数がブール代数になる条件として，以下が知られている．

定理. ブール代数の同値条件
  ハイティング代数 :math:`(H, \sqsubseteq)` について，以下は同値

  1. :math:`H` はブール代数
  2. 任意の :math:`x \in H` について，:math:`x \lor \neg x = \top`
  3. 任意の :math:`x \in H` について，:math:`\neg \neg x = x`

  証明:

  :math:`x \in H` について :math:`x \land \neg x \leq \bot` であること，ブール代数がハイティング代数であることから，1 と 2 の同値性はいいと思う．

  2 から 3 は，

  .. math::

    \neg \neg x = \neg \neg x \land (x \lor \neg x)
    = (\neg \neg x \land x) \lor (\neg \neg x \land \neg x)
    = \neg \neg x \land x

    = (\neg \neg x \land x) \lor (x \land \neg x)
    = (\neg \neg x \lor \neg x) \land x = x

  より示せる．3 から 2 は，ハイティング代数で適用できるド・モルガン則を使用して，

  .. math::

    x \lor \neg x = \neg \neg (x \lor \neg x) = \neg (\neg x \land \neg \neg x) = \neg \bot = \top

  より示せる．

ブール代数では，2つめのド・モルガン則も示せる．

定理. ド・モルガン則 (De Morgan's law)
  ブール代数 :math:`(B, \sqsubseteq)` について、以下が成り立つ:

  * 任意の :math:`x, y \in H` について、:math:`\neg x \lor \neg y = \neg (x \land y)`

  証明:

  ブール代数では二重否定の除去ができることから，ハイティング代数上で成り立つド・モルガン則を使用して，

  .. math::

    \neg x \lor \neg y
    = \neg \neg (\neg x \lor \neg y)
    = \neg (\neg \neg x \land \neg \neg y)
    = \neg (x \land y)

  のように示せる．

さて，ハイティング代数からブール代数への変換方法として，二重否定変換というものが知られている．これは，名前の通り全ての元に二重否定をかますような変換だ．それを示すため，まず準備として補題を用意する．

補題.
  ハイティング代数 :math:`(H, \sqsubseteq)` について，以下が成り立つ:

  * 任意の :math:`x \in H` について，:math:`x \leq \neg \neg x`
  * 任意の :math:`x, y \in H` について，:math:`x \leq y` ならば :math:`\neg y \leq \neg x`

  証明:

  :math:`\neg x \land x \leq \bot` より，:math:`x \leq \neg \neg x`．また，:math:`x \leq y` の時，

  .. math::

    x \land \neg y \leq y \land \neg y \leq \bot

  より，:math:`\neg y \leq \neg x`．

補題.
  ハイティング代数 :math:`(H, \sqsubseteq)` について，以下が成り立つ:

  * 任意の :math:`x \in H` について，:math:`\neg \neg \neg x = \neg x`
  * 任意の :math:`x, y \in H` について，:math:`\neg \neg (x \land y) = \neg \neg x \land \neg \neg y`

  証明:

  前の補題から，:math:`\neg x \leq \neg \neg \neg x`．また，:math:`\neg x \leq \neg \neg x` から :math:`\neg \neg \neg x \leq \neg x`．よって，:math:`\neg \neg \neg x = \neg x`．

  また，:math:`x \land y \leq x`，:math:`x \land y \leq y` から，:math:`\neg \neg (x \land y) \leq \neg \neg x`，:math:`\neg \neg (x \land y) \leq \neg \neg y` より，:math:`\neg \neg (x \land y) \leq \neg \neg x \land \neg \neg y`．さて，後 :math:`\neg \neg x \land \neg \neg y \leq \neg \neg (x \land y)` を示せれば良い．これを示すには，:math:`\neg (x \land y) \land \neg \neg x \land \neg \neg y \leq \bot` を示せれば良い．

  ところで，:math:`\neg (x \land y) \land x \land y \leq \bot` より，

  .. math::

    \neg (x \land y) \land x \leq \neg y = \neg \neg \neg y

  であるため，:math:`\neg (x \land y) \land x \land \neg \neg y \leq \bot` である．同様に :math:`x` についても同じ操作から，:math:`\neg (x \land y) \land \neg \neg x \land \neg \neg y \leq \bot` となる．よって，:math:`\neg \neg (x \land y) = \neg \neg x \land \neg \neg y`．

これより，二重否定変換はハイティング代数上の準同型になり，二重否定の除去を付加できる．これにより，ハイティング代数からブール代数を生成できる．

定理. 二重否定変換 (double-negation translation)
  ハイティング代数 :math:`(H, \sqsubseteq)` について，:math:`H_{\neg\neg} = (\{\neg \neg x \mid x \in H\}, \sqsubseteq)` はブール代数である

  証明:

  前の補題から，元の :math:`H` の :math:`\land` は :math:`H_{\neg\neg}` でも閉じている．また，:math:`x \leq \neg\neg x` より，冪も保存される．よって，:math:`H_{\neg\neg}` はハイティング代数．さらに，:math:`\neg\neg(\neg\neg x) = \neg\neg x` であり，:math:`H_{\neg\neg}` はブール代数になる．

圏論的視点
----------

今までは通常の束論の議論でハイティング代数を取り扱ってきたが，圏論的な議論もしておく．まず，半順序での最大下界，最小上界は，poset での直積，直和になる．これは，定義を比べてみれば分かると思う．そして，最小元，最大元は始対象，終対象に相当する．よって，有界束とは，finitely complete かつ cocomplete な poset のことになる [#notice-finitely-complete]_ ．

定義. 有界束
  poset :math:`C` が有界束とは，以下を満たすこと

  * 有限積を持つ (finitely complete)
  * 有限余積を持つ (finitely cocomplete)

ハイティング代数は，有界束のなかで CCC (Cartesian Closed Category) になるものである．すなわち，冪も持つような poset になる．

定義. ハイティング代数
  有界束 :math:`C` がハイティング代数とは，以下を満たすこと

  * CCC である

冪の定義も照らし合わせてみるとそのままだが，一応確認しておくと，

.. image:: {attach}heyting-algebra-properties/exponential-object.png
  :alt: :math:`C(X \times Y, Z) \simeq C(X, Z^Y)`
  :align: center

が冪対象の定義で，この時 :math:`z^y \land y = z^y \times y \leq z` かつ :math:`x \times y = x \land y \leq z` となる :math:`x` について :math:`x \leq z^y` となる．冪の随伴関係 :math:`C(X \times Y, Z) \simeq C(X, Z^Y)` がそのままで，つまりハイティング代数とは，:math:`- \land Y` が右随伴を持つような有界束のことになる．ついでに，CCC で finite coproduct を持つような圏を BCCC (BiCartesian Closed Category) と言うらしい．ハイティング代数は，この言葉を使うと poset で BCCC になるものと言える．

さて，左随伴の有用な性質として，余極限を保存すると言うものがある．つまり，ハイティング代数では :math:`x \land -` は余極限を保存する．ここから直ちに分配束であることが導ける．

定理.
  ハイティング代数は，分配束である．

  証明:

  :math:`x \land -` は余極限を保存するため，

  .. math::

    x \land \bigvee_y y = \bigvee_y x \land y

  である．

ところで，:math:`\neg` はハイティング代数 :math:`H` 上の反変関手 :math:`\neg: H \to H^{\mathrm{op}}` になる．逆にも :math:`\neg: H^{\mathop{op}} \to H` を張れて，こいつらは随伴になる．一般に，:math:`H` の対象 :math:`X` について，:math:`X^-: H \to H^{\mathrm{op}}` は左随伴になる．

定理.
  ハイティング代数 :math:`H`，:math:`X \in |H|` について，:math:`X^-: H \to H^{\mathrm{op}}` は右随伴を持つ．

  証明:

  .. math::

    H^{\mathrm{op}}(X^Y, Z)
    \simeq H(Z, X^Y)
    \simeq H(Y \times Z, X)
    \simeq H(Y, X^Z)

  より，:math:`X^-: H^{\mathrm{op}} \to H` が右随伴になり，:math:`X^- \dashv X^-: H \to H^{\mathrm{op}}` になる．

ところで，:math:`\neg: H \to H^{\mathrm{op}}` が左随伴を持つ場合がある．:math:`\neg: H^{\mathrm{op}} \to H` がその左随伴になる :math:`H` がブール代数である．

定義. ブール代数
  ハイティング代数 :math:`H` がブール代数とは，以下を満たすこと:

  * :math:`\neg \dashv \neg: H^{\mathrm{op}} \to H`

この定義がブール代数の条件に合うことを確認する前に少々寄り道する．さて，随伴同値 (adjoint equivalence) と言う条件がある．これは，随伴による単子 (unit)，余単子 (counit) が自然同型になることである．

定義. 随伴同値
  随伴 :math:`F \dashv G` について，その単子，余単子が自然同型である時，:math:`F \dashv G` を随伴同値という．

随伴同値の場合，:math:`F \dashv G` かつ :math:`G \dashv F` になる．

定理.
  随伴 :math:`F \dashv G` が随伴同値ならば，:math:`G \dashv F` である

  証明:

  unit :math:`\eta: 1 \Rightarrow G \circ F`，counit :math:`\epsilon: F \circ G \Rightarrow 1` が自然同型である時，それぞれの逆射 :math:`\epsilon^{-1}: 1 \Rightarrow F \circ G`，:math:`eta^{-1}: G \circ F \Rightarrow 1` が存在する．この時，

  * :math:`G \xrightarrow{G \epsilon^{-1}} GFG \xrightarrow{\eta^{-1}_G} G`
  * :math:`F \xrightarrow{\epsilon^{-1}_F} FGF \xrightarrow{F \eta^{-1}} F`

  はそれぞれ

  * :math:`F \xrightarrow{F \eta} FGF \xrightarrow{\epsilon_F} F`
  * :math:`G \xrightarrow{\eta_G} GFG \xrightarrow{G \epsilon} G`

  の逆射になる．よって恒等射になるため，それぞれが unit / counit になる随伴 :math:`G \dashv F` が作れる．

ところで，:math:`\neg \circ \neg` は恒等関手，つまり二重否定の除去が成り立つとすると，この時 :math:`\neg \dashv \neg: H \to H^{\mathrm{op}}` の unit は自然同型になる．また，unit が自然同型になるならば二重否定の除去ができる．つまりブール代数とは，ハイティング代数で :math:`\neg` が随伴同値になるものである [#notice-adjoint-equivalence-and-self-adjoint]_ ．

定理. ブール代数の条件
  ハイティング代数 :math:`H` について，以下は同値

  1. :math:`H` はブール代数
  2. :math:`\neg \circ \neg: H \to H` は恒等関手
  3. :math:`\neg \dashv \neg: H \to H^{\mathrm{op}}` は随伴同値

  証明:

  3 から 1 はよい．1 から 2 は，

  * :math:`H(Y, \neg X) \simeq H^{\mathrm{op}}(\neg X, Y) \simeq H(X, \neg Y)`
  * :math:`H(\neg X, Y) \simeq H^{\mathrm{op}}(X, \neg Y) \simeq H(\neg Y, X)`

  より，:math:`\neg X \xrightarrow{1} \neg X` に対応する射

  * :math:`X \leq \neg\neg X`
  * :math:`\neg\neg X \leq X`

  が存在する．よって，:math:`\neg\neg X = X` であることより，示せる．2 から 3 は，unit / counit が恒等関手から恒等関手への口頭変換になることから自明．

ところで，ブール代数に限らずハイティング代数では :math:`\neg \dashv \neg` だった．随伴からはモナドが作れるので，:math:`\neg\neg` はモナドになる．この時，:math:`\neg\neg(\neg\neg x) \leq \neg\neg x` がモナドの自然変換から作れるため，:math:`\neg\neg` により作られる subcategory がハイティング代数であれば，:math:`\neg\neg(\neg\neg x) = \neg\neg x` になる．つまり，:math:`\neg\neg` により作られる subcategory はブール代数になる．:math:`\neg\neg` により作られる subcategory がハイティング代数であることは，finite product / finite coproduct / implication を保存することを地道に証明する方法しか知らないので割愛する．誰か他の方法知ってたら教えて欲しい [#notice-double-negation]_．

まとめ
------

てことで，ハイティング代数は BCCC になる poset で，ブール代数は :math:`0^-` が随伴同値になるハイティング代数だよ，何か問題でも?

なお，:math:`X^-` が左随伴になるって話から，:math:`(x \lor y) \Rightarrow z = (x \Rightarrow z) \land (y \Rightarrow z)` が直ちに示せたり [#notice-exponential-contravariance]_，随伴 :math:`\neg \dashv \neg` の同値性 :math:`H(Y, \neg X) \simeq H(X, \neg Y)` に :math:`X = \neg Z`，:math:`Y = Z` すれば :math:`Z \leq \neg\neg Z` が直ちに示せたりして便利．まあ，その証明が相手に通じるかは置いといて．てことで，今回は以上．

.. [#notice-finitely-complete] finitely complete になるには厳密には，finite product と equalizer の存在が必要になる．ただ，今回は poset なので，任意の対象間に射はたかだか一つしか存在しないため，equalizer は自明に存在する．finitely cocomplete も同様．
.. [#notice-adjoint-equivalence-and-self-adjoint] 一般には，:math:`F \dashv G`，:math:`G \dashv F` の時に :math:`F \dashv G` が随伴同値になるとは限らない．
.. [#notice-exponential-contravariance] :math:`X^-` は反変であることに注意．反変なので，product / coproduct が逆転する．
.. [#notice-double-negation] 某勉強会でそのうち出てくる?
