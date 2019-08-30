Finitary Projection と正規部分集合
==================================

:date: 2019-08-11 22:00
:tags: Domain Theory, cpo, 表示的意味論, 本
:category: 数学

Semantics of Programming Languages (SoPL) の読書会に参加してるんだけど，最近これどうだったけみたいなことが多いので，ちょっとまとめとく (特に証明ないやつ)．

出典は，

  Carl A. Gunter. (1992). Semantics of Programming Languages: Structures and Techniques. Foundations of Computing. Retrieved from https://mitpress.mit.edu/books/semantics-programming-languages

で，証明書いてないやつを書いたり，本文にあるやつにもうちょっと追記したりしてる．追記部分は間違いがあるかもなので，そこは注意してくれ．基本用語の定義は `CPO に関連する定義まとめ <{filename}/articles/2019/05/cpo-related-definitions.rst>`_ に則る．

finitary projection と 正規部分集合
-----------------------------------

finitary projection とはドメインからドメインへの連続な射影のこと [#typo-finitary-projection-in-sopl]_．

定義. finitary projection
  ドメイン :math:`D` について，連続関数 :math:`p: D \to D` が以下を満たす時， finitary projection と言う:

  * :math:`p \circ p = p \sqsubseteq \mathrm{id}`
  * :math:`\mathrm{Im}(p)` はドメイン

  :math:`D` をドメインとした時， :math:`\mathrm{Fp}(D)` を :math:`D` 上の finitary projection の集合とする．

finitary projection は冪等性を持つため，不動点の集合は像になる．

補題. finitary projection の不動点
  任意の finitary projection :math:`p: D \to D` ， :math:`x \in D` について，

  .. math::

    p(x) = x \iff x \in \mathrm{Im}(p)

  証明:

  :math:`\implies` は自明なので良い．逆は，

  .. math::

    x \in \mathrm{Im}(p)
    \implies \exists y \in D\ldotp x = p(y)

  で，この :math:`y` について，

  .. math::

    p(x) = p(p(y)) = (p \circ p)(y) = p(y) = x

まず， finitary projection の像の性質を見る．

定理. finitary projection の像の正規性
  :math:`D` をドメインとする．この時，以下が成り立つ:

  1. 任意の :math:`p, q \in \mathrm{Fp}(D)` について， :math:`p \sqsubseteq q \iff \mathrm{Im}(p) \subseteq \mathrm{Im}(q)` ．
  2. 任意の :math:`p \in \mathrm{Fp}(D)` について， :math:`\mathrm{K}(\mathrm{Im}(p)) = \mathrm{Im}(p) \cap \mathrm{K}(D)` ．

  証明:

  1 は，以下のように証明できる:

  :math:`p \sqsubseteq q` の時，任意の :math:`x \in \mathrm{Im}(p)` について，

  .. math::

    x = p(x) \sqsubseteq q(x) \sqsubseteq x

  より， :math:`x = q(x) \in \mathrm{Im}(q)` ．また， :math:`\mathrm{Im}(p) \subseteq \mathrm{Im}(q)` の時，任意の :math:`x \in D` について，

  .. math::

    p(x) = q(p(x)) \sqsubseteq q(x)

  :math:`\mathrm{Im}(p) \subseteq \mathrm{Im}(q)` なので， :math:`p(x) \in \mathrm{Im}(q)` に注意．

  2 は，以下のように証明できる:

  :math:`p(x) \in \mathrm{K}(\mathrm{Im}(p))` について， :math:`p(x) \in \mathrm{Im}(p)` ．また， directed subset :math:`M \subseteq D` について， :math:`p(x) \sqsubseteq \bigsqcup M` の時，

  .. math::

    p(x) = p(p(x)) \sqsubseteq p(\bigsqcup M) = \bigsqcup p(M)

  :math:`p(M)` は :math:`\mathrm{Im}(p)` の directed subset になるので， :math:`p(x)` のコンパクト性から :math:`p(x) \sqsubseteq p(y) \sqsubseteq y` となる :math:`y \in M` が存在する．よって， :math:`p(x)` は :math:`D` でもコンパクト．

  :math:`p(x) \in \mathrm{Im}(p) \cap \mathrm{K}(D)` について，任意の directed subset :math:`M \subseteq \mathrm{Im}(p) \subseteq D` で， :math:`p(x) \sqsubseteq \bigsqcup M \implies \exists y \in M\ldotp x \sqsubseteq y` なので， :math:`p(x)` は :math:`\mathrm{Im}(p)` でもコンパクトになる．一般にコンパクト性は部分集合に範囲を狭めても成り立つ．

1 番目は像が単調性を保存することを示し， 2 番目は像でのコンパクト元が元のドメインでのコンパクト元でもあることを示している．ここから，以下のことが言える．

補題. finitary projection によるドメイン
  :math:`\mathrm{Fp}(D)` はドメイン

  証明:

  directed subset :math:`M \subseteq \mathrm{Fp}(D)` を考える．この時， :math:`p(x) = \bigsqcup_{q \in M} q(x)` が :math:`p = \bigsqcup M \in \mathrm{Fp}(D)` であることを示す．連続関数の lub なので連続であること :math:`\bigsqcup_{q \in M} q(x) \sqsubseteq x` より :math:`\mathrm{id}` より小さいことはいいだろう．冪等性は，

  .. math::

    p(x) = \bigsqcup_{q \in M} q(x) = \bigsqcup_{q \in M} q(q(x))
    \sqsubseteq \bigsqcup_{q_1 \in M} q_1(\bigsqcup_{q_2 \in M} q_2(x)) = p(p(x))
    \sqsubseteq \bigsqcup_{q \in M} q(x) = p(x)

  より成り立つ．また，

  .. math::

    p(x) = \bigsqcup_{q \in M} q(x) = \bigsqcup_{q \in M} \bigsqcup \downarrow q(x) \cap \mathrm{K}(\mathrm{Im}(q))
    \sqsubseteq \bigsqcup \downarrow \bigsqcup_{q \in M} q(x) \cap \mathrm{K}(\mathrm{Im}(p))
    = \bigsqcup \downarrow p(x) \cap \mathrm{K}(\mathrm{Im}(p))
    \sqsubseteq \bigsqcup \downarrow p(x) \cap \mathrm{K}(D) = p(x)

  より :math:`\mathrm{Im}(p)` はドメインになる．で， :math:`M` は連続関数の集合より :math:`\bigsqcup M = p \in \mathrm{Fp}(D)` である．

ところで， poset についての正規性は，以下のように定義される．

定義. 正規部分集合 (normal subset)
  poset :math:`P` について， :math:`N \subseteq P` が以下を満たす時， :math:`N` を :math:`P` の normal subset という．

  .. math::

    \forall x \in P\ldotp \text{$\downarrow x \cap N$ は directed}

  この時， :math:`N \lhd P` と表記する．

実は， finitary projection の像はコンパクト元全体の normal subset に対応することが示せる．

定理. finitary projection と normal subset の対応
  任意のドメイン :math:`D` に対し， :math:`\mathrm{Fp}(D)` と :math:`\{D \mid D \lhd \mathrm{K}(D)\}` に包含関係を入れた poset は同型．

  証明:

  それぞれの対応は以下のように作る:

  * 任意の :math:`p \in \mathrm{Fp}(D)` について， :math:`\mathrm{K}(\mathrm{Im}(p)) \lhd \mathrm{K}(D)`
  * 任意の :math:`N \lhd \mathrm{K}(D)` について， :math:`(x \mapsto \bigsqcup \downarrow x \cap N) \in \mathrm{Fp}(D)`

  まずそれぞれが条件を満たすことを確認する．

  :math:`p \in \mathrm{Fp}(D)` について， :math:`N = \mathrm{K}(\mathrm{Im}(p))` とおく． :math:`N = \mathrm{Im}(p) \cap K(D) \subseteq \mathrm{K}(D)` なことは良い．任意の :math:`x \in \mathrm{K}(D)` について， :math:`M = \downarrow x \cap N` を考える．ところで，任意の :math:`z \in M` について :math:`z \sqsubseteq x` ， :math:`z \in \mathrm{Im}(p)` から :math:`z = p(z) \sqsubseteq p(x)` ．よって， :math:`M = \downarrow p(x) \cap \mathrm{K}(\mathrm{Im}(p))` ． :math:`\mathrm{Im}(p)` はドメインより， :math:`M` は directed ．よって， :math:`N \lhd \mathrm{K}(D)` ．

  また， :math:`N \lhd \mathrm{K}(D)` について， :math:`p(x) = \bigsqcup \downarrow x \cap N` とおく．まず連続性を見る． :math:`x \sqsubseteq y \in D` について， :math:`\downarrow x \cap N \subseteq \downarrow y \cap N` より，

  .. math::

    p(x) = \bigsqcup \downarrow x \cap N \sqsubseteq \bigsqcup \downarrow y \cap N = p(y)

  また， directed subset :math:`P \subseteq D` について，任意の :math:`x \in P` で単調性から :math:`p(x) \sqsubseteq p(\bigsqcup P)` より :math:`p(\bigsqcup P)` は :math:`p(P)` の上限なので :math:`\bigsqcup p(P) \sqsubseteq p(\bigsqcup P)` ．また任意の :math:`y \in  \downarrow (\bigsqcup P) \cap N` について考えると， :math:`y \in N \subseteq \mathrm{K}(D)` で :math:`y \sqsubseteq \bigsqcup P` より， :math:`y \sqsubseteq z` となる :math:`z \in P` が存在し， :math:`y \in \downarrow z \cap N`　より :math:`y \sqsubseteq \bigsqcup \downarrow z \cap N = p(z) \sqsubseteq \bigsqcup p(P)` ．ここから :math:`\bigsqcup p(P)` は :math:`\downarrow (\bigsqcup P) \cap N` の上限なので， :math:`p(\bigsqcup P) = \bigsqcup \downarrow (\bigsqcup P) \cap N \sqsubseteq \bigsqcup p(P)` ．よって， :math:`p(\bigsqcup P) = \bigsqcup p(P)` ．

  また，任意の :math:`x \in D` について，

  .. math::

    p(p(x))
    = p(\bigsqcup \downarrow x \cap N)
    = \bigsqcup p(\downarrow x \cap N)
    = \bigsqcup_{z \in \downarrow x \cap N} \bigsqcup \downarrow z \cap N

  ここで :math:`z \in N` で :math:`\bigsqcup \downarrow z \cap N = z` より，

  .. math::

    = \bigsqcup \downarrow x \cap N
    = p(x)

  また， :math:`x` は :math:`\downarrow x \cap N` の上限より :math:`p(x) = \bigsqcup \downarrow x \cap N \sqsubseteq x` ．

  最後に :math:`\mathrm{Im}(p)` がドメインであることを見る．任意の directed subset :math:`p(P) \subseteq \mathrm{Im}(p)` について， :math:`\bigsqcup p(P) = p(\bigsqcup P) \in \mathrm{Im}(p)` ．また :math:`p(x) \in \mathrm{Im}(p)` について， :math:`p(x) \sqsubseteq x` より :math:`\downarrow p(x) \cap \mathrm{K}(\mathrm{Im}(p)) \subseteq \downarrow x \cap \mathrm{K}(\mathrm{Im}(p))` ．また，任意の :math:`p(y) \in \downarrow x \cap \mathrm{K}(\mathrm{Im}(p))` について， :math:`p(y) \sqsubseteq x` より :math:`p(y) = p(p(y)) \sqsubseteq p(x)` より :math:`p(y) \in \downarrow p(x) \cap \mathrm{K}(\mathrm{Im}(p))` ．ここから，

  .. math::

    \bigsqcup \downarrow p(x) \cap \mathrm{K}(\mathrm{Im}(p))
    = \bigsqcup \downarrow x \cap \mathrm{K}(\mathrm{Im}(p))

  ここで :math:`y \in N` ， directed subset :math:`p(P) \subseteq \mathrm{Im}(p)` について， :math:`y \sqsubseteq \bigsqcup p(P) = p(\bigsqcup P)` の時， :math:`y \sqsubseteq p(\bigsqcup P) \sqsubseteq \bigsqcup P` ， :math:`y \in N \sqsubseteq \mathrm{K}(D)` より， :math:`y \sqsubseteq z` となる :math:`z \in P` が存在する． :math:`y = p(y) \sqsubseteq p(z) \in p(P)` より :math:`y \in \mathrm{K}(\mathrm{Im}(p))` ．また， :math:`p(y) \in \mathrm{K}(\mathrm{Im}(p))` について， :math:`p(y) = \bigsqcup \downarrow y \cap N` ．ところで， finitary projection の像の正規性の補題で :math:`K(\mathrm{Im}(p)) = \mathrm{Im}(p) \cap \mathrm{K}(D)` を示したが，この証明に :math:`\mathrm{Im}(p)` がドメインという条件は必要なかった．なので， :math:`p(y) \in \mathrm{K}(D)` より， :math:`p(y) = z \in N` となるような :math:`z \in \downarrow y \cap N` が存在する．よって， :math:`K(\mathrm{Im}(p)) = N` より，

  .. math::

    = \bigsqcup \downarrow x \cap N
    = p(x)

  最後に同型となっていることを確かめる．任意の :math:`p \in \mathrm{Fp}(D)` ， :math:`x \in D` について，

  .. math::

    \bigsqcup \downarrow x \cap \mathrm{K}(\mathrm{Im}(p))
    = \bigsqcup \downarrow p(x) \cap \mathrm{K}(\mathrm{Im}(p))
    = p(x)

  また，任意の :math:`N \lhd \mathrm{K}(D)` について，上述の通り :math:`\mathrm{K}(\mathrm{Im}(x \mapsto \bigsqcup \downarrow x \cap N)) = N` ．

コンパクト元で構成される有限な normal subset は，あるドメインの解析に役に立つ．それをこのあと紹介する．

ep ペアと bifinite ドメイン
---------------------------

ep ペアは，対応する連続な埋め込みと射影の組．

定義. ep (embedding-projection) pair
  cpo :math:`D` ， :math:`E` に対し，連続関数 :math:`e: D \to E` ， :math:`p: E \to D` の組 :math:`(e, p)` が， :math:`p \circ e = \mathrm{id}` ， :math:`e \circ p \sqsubseteq \mathrm{id}` を満たす時， :math:`(e, p)` を ep (embedding-projection) pairと呼ぶ．この時， :math:`e` を埋め込み (embedding) ， :math:`p` を射影 (projection) と呼ぶ．

なお，一般に埋め込みに対して射影が存在するとは限らないし，逆もそう．ここでいう，埋め込みと射影とは一般での意味と異なるので注意．

補題. 恒等写像と合成の保存
  * 任意の cpo :math:`D` について， :math:`(\mathrm{id}: D \to D, \mathrm{id}: D \to D)` は ep ペア．
  * 任意の cpo :math:`D_1` ， :math:`D_2` ， :math:`D_3` について， ep ペア :math:`(e_1: D_1 \to D_2, p_1: D_2 \to D_1)` ， :math:`(e_2: D_2 \to D_3, p_2: D_3 \to D_2)` の合成 :math:`(e_2 \circ e_1: D_1 \to D_3, p_1 \circ p_2: D_3 \to D_1)` は ep ペア．

  証明:

  :math:`\mathrm{id}` の方はいいと思う．合成の方は，

  .. math::

    (p_1 \circ p_2) \circ (e_2 \circ e_1) = p_1 \circ (p_2 \circ e_2) \circ e_1 = p_1 \circ e_1 = \mathrm{id}

    (e_2 \circ e_1) \circ (p_1 \circ p_2) = e_2 \circ (e_1 \circ p_1) \circ p_2 \sqsubseteq e_2 \circ p_2 \sqsubseteq \mathrm{id}

  連続関数において， :math:`f_1 \sqsubseteq f_2` ， :math:`g_1 \sqsubseteq g_2` の時， :math:`f_1 \circ g_1 \sqsubseteq f_2 \circ g_2` に注意．

ここから， cpo enriched な圏に対して ep ペアを持ってきて圏を構成することができる． ep ペアを射と見て :math:`f = (e: D \to E, p: E \to D)` を :math:`f: D \to E` と表記することもある．この時， :math:`f^e = e` ， :math:`f^p = p` という表記も用いる． :math`\mathrm{Cpo}` enriched な圏 :math:`C` に対して， ep ペアを射とした圏を :math:`C^{\mathit{ep}}` と表記する． ep ペアの埋め込みは，コンパクト性を保存する．

補題. 埋め込みによるコンパクト性の保存
  ep ペア :math:`f: D \to E` において， :math:`f^e(\mathrm{K}(D)) \subseteq \mathrm{K}(E)` ．

  証明:

  任意の :math:`x \in \mathrm{K}(D)` ， directed subset :math:`M \subseteq E` を考える． :math:`f^e(x) \sqsubseteq \bigsqcup M` と仮定すると， :math:`f^p` の連続性より，

  .. math::

    f^p(f^e(x)) = x \sqsubseteq f^p(\bigsqcup M) = \bigsqcup f^p(M)

  :math:`x` はコンパクトなので， :math:`x \sqsubseteq f^p(y)` となる :math:`y \in M` が存在する．この時，

  .. math::

    f^e(x) \sqsubseteq f^e(f^p(y)) \sqsubseteq y

  より， :math:`f^e(x)` はコンパクトになる．

また，射影は全射になる．

補題. 射影は全射
  ep ペア :math:`f: D \to E` において， :math:`\mathrm{Im}(f^p) = D` ．

  証明:

  :math:`f^p \circ f^e = \mathrm{id}_D` より，

  .. math::

    D = \mathrm{Im}(\mathrm{id}_D) = f^p(\mathrm{Im} f^e) \subseteq \mathrm{Im}(f^p) \subseteq D

補題. ep ペアによる同型
  ep ペア :math:`f: D \to E` において， :math:`D` と :math:`\mathrm{Im}(f^e)` は同型

  証明:

  :math:`D \xrightarrow{f^e} \mathrm{Im}(f^e) \xrightarrow{f^p} D = D \xrightarrow{\mathrm{id}} D` なのは良い． :math:`f^e(x) \in \mathrm{Im}(f^e)` に対して，

  .. math::

    f^e(f^p(f^e(x))) = f^e(x)

  より， :math:`\mathrm{Im}(f^e) \xrightarrow{f^p} D \xrightarrow{f^p} \mathrm{Im}(f^e) = \mathrm{Im}(f^e) \xrightarrow{\mathrm{id}} \mathrm{Im}(f^e)` で，それぞれの射は連続より同型射になる．

ところで， ep ペアによって構成された圏から，以下の制約を考えることができる．

定義. bifinite cpo
  ある cpo :math:`D` が bifinite とは，有限な poset による diagram から作られる :math:`\mathrm{Cpo}^{\mathit{ep}}` 上の directed colimit であることを指す．

ただし， :math:`\mathrm{Cpo}` とは cpo が対象で連続関数が射となる圏．この時， :math:`\mathrm{Cpo}^{\mathit{ep}}` とは， cpo が対象で射が ep ペアとなる圏になる．なお， bifinite cpo による圏 :math:`\mathrm{Bif}` を考えることができ，こいつは :math:`\mathrm{Cpo}` の full subcategory になる． finitary projection は bifinite cpo の basis を解析するのに役に立つ．

bifinite ドメインの基底
-----------------------

ドメインから cpo への ep ペアからは， finitary projection が構成できる．

補題. ep ペアからの finitary projection の構成
  ドメイン :math:`D` とcpo :math:`E` について， ep ペア :math:`f: D \to E` を考える．この時， :math:`f^e \circ f^p` は finitary projection ．

  証明:

  :math:`p = f^e \circ f^p` とおく．連続なことと :math:`\mathrm{id}` より小さいことは良い．また，

  .. math::

    p \circ p = f^e \circ (f^p \circ f^e) \circ f^p = f^e \circ f^p = p

  より冪等性も成り立つ．最後に :math:`\mathrm{Im}(p)` がドメインであることを見る．

  .. math::

    \mathrm{Im}(p) = f^e(\mathrm{Im}(f^p)) = f^e(D) = \mathrm{Im}(f^e)

  で :math:`\mathrm{Im}(f^e)` は :math:`D` と同型より， :math:`\mathrm{Im}(p)` はドメインになる．

逆に finitary projection からも ep ペアが構成できる．

補題. finitary projection からの ep ペアの構成
  finitary projection :math:`p: D \to D` に対し， :math:`(\mathrm{id}_{\mathrm{Im}(p)}\uparrow D: \mathrm{Im}(p) \to D, p\downarrow\mathrm{Im}(p): D \to \mathrm{Im}(p))` は ep ペア．

  証明:

  :math:`f = (\mathrm{id}_{\mathrm{Im}(p)}\uparrow D: \mathrm{Im}(p) \to D, p\downarrow\mathrm{Im}(p): D \to \mathrm{Im}(p))` とおく．

  .. math::

    \forall x \in D\ldotp (f^e \circ f^p)(x) = p(x) \sqsubseteq x

  なのはいいだろう．逆は，

  .. math::

    \forall p(x) \in \mathrm{Im}(p)\ldotp (f^p \circ f^e)(p(x)) = p(p(x)) = p(x)

  になるので， :math:`f` は ep ペアになる．

ここから ep ペアの colimit の lub は :math:`\mathrm{id}` になることが示せる．

補題. bifinite ドメイン
  bifinite cpo :math:`D` はドメインである．

  証明:

  :math:`\mu: \{\Delta_i\}_{i \in I} \to D` を bifinite cpo の colimit cone とする．任意の :math:`x \in D` について，

  .. math::

    \bigsqcup \downarrow x \cap \mathrm{K}(D) =

補題. colimit cone の lub
  :math:`\mathrm{Alg}^{\mathit{ep}}` 上の directed colimit :math:`\mu: \{\Delta_i\}_{i \in I} \to D` について， :math:`\bigsqcup_{i \in I} \mu_i^e \circ \mu_i^p = \mathrm{id}_D`

  証明:

  :math:`\bigsqcup_{i \in I} \mu_i^e \circ \mu_i^p` は finitary projection で，

bifinite ドメイン上では， finite poset をコンパクト元のように見ることができる．

補題. bifinite ドメイン上のコンパクト性
  :math:`\mu: {(\Delta_i)}_I \to D` を :math:`\mathrm{Bif}^{\mathit{ep}}` 上の directed colimit とする．この時， :math:`E` を finite poset ， :math:`f: E \to D` を ep ペアとした時， :math:`f = \mu_i \circ g` を満たす :math:`i \in I` 及び ep ペア :math:`g: E \to \Delta_i` が存在する．

  証明:

  :math:`p = f^e \circ f^p` を考える．

  .. math::

    \mathrm{Im}(p) = f^e(\mathrm{Im}(f^p)) = f^e(E) = \mathrm{Im}(f^e)

  であり， :math:`p` は finitary projection なので， :math:`\mathrm{Im}(f^e) \lhd \mathrm{K}(D)` ．同様に :math:`\hat{\mu_i} = \mu_i^e \circ \mu_i^p` も finitary projection で :math:`\mathrm{Im}(\hat{\mu_i}) \lhd \mathrm{K}(D)` ．

  ここから， :math:`g_i = (\mu_i^p \circ f^e, f^p \circ \mu_i^e)` とおくと条件を満たす ep ペアになる．まず， ep ペアになることを見る．連続なのはいいだろう．

  .. math::

    g^e \circ g^p = \mu_i^p \circ f^e \circ f^p \circ \mu_i^e \sqsubseteq \mu_i^p \circ \mu_i^e = \mathrm{id}

  であり， :math:`\mathrm{Im}(f^e) \subseteq \mathrm{Im}(\mu_i^e) = \mathrm{Im}(\hat{\mu_i})` から :math:`\hat{mu_i}(f^e(x)) = f^e(x)` より，

  .. math::

    (g^p \circ g^e)(x)
    = f^p(\mu_i^e(\mu_i^p(f^e(x))))
    = f^p(\hat{\mu_i}(f^e(x)))
    = f^p(f^e(x))
    = \mathrm{id}

  また，

  .. math::

    \mu_i \circ g
    = (\mu_i^e \circ g^e, g^p \circ \mu_i^p)
    = (\mu_i^e \circ \mu_i^p \circ f^e, f^p \circ \mu_i^e \circ \mu_i^p)

  (TODO)

定理. ep ペア上の directed colimit
  :math:`\mathrm{Cpo}` enriched な圏 :math:`C` に対して， :math:`\Delta` を :math:`C^{\mathit{ep}}` 上の directed diagram とした時， cocone :math:`\mu: \Delta \to D` について，

  .. math::

    \bigsqcup \mu_i^e \circ \mu_i^p = \mathrm{id}_D \implies \text{$\mu$ は colimit}

定理. bifinite ドメインの基底
  :math:`D` を cpo とし， :math:`M = \{p \in \mathrm{Fp}(D) \mid \text{$\mathrm{Im}(p)$ は有限}\}` とおく．この時，以下は同値．

  1. :math:`D` は bifinite
  2. :math:`M` は directed で， :math:`\bigsqcup M = \mathrm{id}_D`
  3. :math:`D` はドメインで，任意の有限集合 :math:`U \subseteq \mathrm{K}(D)` について :math:`U \subseteq N \lhd \mathrm{K}(D)` を満たす有限集合 :math:`N` が存在する．

  証明:

  まず， :math:`\text{1} \implies \text{2}` を示す． :math:`\mu: \Delta \to D` を colimit cone とする．この時， :math:`\Delta_i` は有限 poset である．

まとめ
------

.. [#typo-finitary-projection-in-sopl] SoPL では実は finitary projection に連続性が含まれていないんだけど，多分タイポだと思う．
