colimit としての自然数の集合
============================

:date: 2019-08-30 22:30
:tags: Domain Theory, 束論, 順序理論
:category: 数学

:math:`F; i \mapsto \{n \mid n \leq i\}: \mathbb{N} \to \mathrm{Poset}` みたいな directed diagram を考えた時，こいつの colimit は何になるか考えようという話が降ってきたんだけど，圏論弱者で頭が混乱してしまって解けなかったので，色々考えたメモ．

directed colimit
----------------

ある圏 :math:`C` と圏 :math:`I` からの図式 :math:`F: I \to C` を考える．この時， :math:`F` の colimit とは， :math:`C` の対象 :math:`\mathop{\mathrm{colim}} F \in |C|` と cocone :math:`\mu: F \Rightarrow \Delta (\mathop{\mathrm{colim}} F)` の組で，以下の普遍性を満たすもの:

.. math::

  \forall \varphi: F \Rightarrow \Delta X\ldotp \exists ! f: \mathop{\mathrm{colim}} F \Rightarrow X\ldotp \mu; \Delta(f) = \varphi

ただし， :math:`\Delta: C \to C^I` は対角関手．

diagram の index が directed set である場合に，それ上の colimit を directed colimit と呼ぶ．とりあえずここまでが導入．

colimit としての :math:`\mathbb{N}`
-----------------------------------

:math:`F; i \to \{n \mid n \leq i\}: \mathbb{N} \to \mathrm{Poset}` のような diagram を考える．射は単純に :math:`F(i \leq j); n \mapsto n` で作れる．こいつの directed colimit は何になるだろう？ 直感的には，最終的に :math:`\mathbb{N}` に収束しそうだ．確かめてみる．

まず colimit cocone は :math:`\mu_i; x \mapsto x: \{n \mid n \leq i\} \to \mathbb{N}` で考えられそうだ．さて， :math:`\varphi: F \Rightarrow \Delta X` に対して， :math:`\mu; \Delta(f) = \varphi` を満たす :math:`f: \mathbb{N} \to X` を考えてみる．

.. image:: {attach}set-of-natural-numbers-is-colimit/poset-colimit-proof.png
  :alt: 一意性について，計算している図式．
  :align: center

つまり任意の :math:`f` について， :math:`f(n) = \varphi_n(n)` が成り立ち，逆に :math:`n \mapsto \varphi_n(n)` を考えればこれは一意で等式を満たす射ということになる．ここから， :math:`\mathbb{N}` は :math:`F` の colimit となる．

Cpo での colimit
----------------

さて実は本題はここからで， :math:`\{n \mid n \leq i\}` は finite poset でありつまり cpo だ．なので先ほどの :math:`F` は :math:`\mathbb{N}` から :math:`\mathrm{Cpo}` への図式としても考えられる．この場合， :math:`F` の colimit は何になるだろう？ 問題は， :math:`\mathbb{N}` は cpo でないことだ． :math:`\mathbb{N}` 全体を考えると，こいつは directed set になるが sup を持たないからだ．では， :math:`F` の colimit は無いのだろうか？

実は， :math:`\mathbb{N}` は cpo では無いのだが :math:`\mathbb{N}_{\infty} = \mathbb{N} \cup \{\infty\}` は cpo になる．こいつに対して :math:`\mathrm{Poset}` での構成を試してみる． :math:`\mathbb{N}` の範囲では同じ話を適用できる．が， :math:`f(\infty)` が一意に決まるかは分からないので， colimit を考えられないように見える．しかし，実はこいつを決められる要素が， :math:`\mathrm{Cpo}` にはある．それが連続性だ．今回の場合 :math:`f: \mathbb{N}_{\infty} \to X` は連続関数なので，次の式が成り立つ:

.. math::

  f(\infty) = f(\bigsqcup_{n \in \mathbb{N}} n) = \bigsqcup_{n \in \mathbb{N}} f(n) = \bigsqcup_{n \in \mathbb{N}} \varphi_n(n)

を満たす必要がある． :math:`X` が cpo なので，こいつは well-defined で sup は一意より， :math:`f` は :math:`\mathbb{N}_{\infty}` 全域で一意に定めることができる．ここから， :math:`F: \mathbb{N} \to \mathrm{Cpo}` の colimit は :math:`\mathbb{N}_{\infty}` になる．

:math:`\mathrm{Poset}` と :math:`\mathrm{Cpo}`
----------------------------------------------

ここまで見て，混乱した人はいないだろうか？ 僕は当初混乱した． :math:`\mathbb{N}_{\infty}` が :math:`\mathrm{Cpo}` で colimit になれるなら，なぜ :math:`\mathrm{Poset}` では慣れなかったのだろう？ もしかして， :math:`\mathbb{N}` と :math:`\mathbb{N}_{\infty}` って poset として同型なのか？ とか本気で考えた (そんなバカな話はもちろん成り立たない) ．なので，最後に :math:`\mathrm{Poset}` と :math:`\mathrm{Cpo}` で何が違うのか見てみる．

:math:`\mathrm{Poset}` の中で :math:`\mathbb{N}_{\infty}` から :math:`\mathbb{N}` への一意射が作れないか考えてみる．先ほどの話から，問題は :math:`f(\infty)` の定め方にあることは分かるだろう．こいつをどう定めるかが問題になるわけだけど， :math:`\mathrm{Poset}` の射は単調である必要があるので，

.. math::

  \forall n \in \mathbb{N}\ldotp f(\infty) \geq f(n) = n

を満たす必要がある．ここまでくるとみなさんお気づきだと思うけど， :math:`\mathbb{N}` は上限を持たないので，これを満たすように :math:`f` をうまく作れないのだ．これが， :math:`\mathrm{Poset}` と :math:`\mathrm{Cpo}` の大きな違いっぽくて， :math:`\mathrm{Cpo}` は directed subset でさえ sup を持てないようなものを除外することで well-defined 性をうまく保証してくれるっぽい．なので，連続性を用いて一意にうまく定めることができるけど， :math:`\mathrm{Poset}` ではそこがうまくいかない，一意射の一意性どころか構成できないというわけだ．

まとめ
------

これは完全に知らない話だったので，ほへーってなった．また一つ賢くなってしまった (順序理論界隈とかだと常識かもしれないので，無知を晒してしまったとも言う)． :math:`\mathrm{Cpo}` は :math:`\mathrm{Poset}` の full subcategory だと無邪気に信じてたが，こうなると怪しそう．埋め込みうまく作る方法あったりするんだろうか？ 時間ができたらちょっと考えてみたい．
