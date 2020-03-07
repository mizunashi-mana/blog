LaTeX の数式フォントに STIX を使う
==================================

:date: 2020-03-07 15:00
:tags: LaTeX, フォント, 数式, STIX
:category: ツール

LaTeX では Computer Modern フォントが使われている．ただ，Computer Modern フォントはかなり古く対応してる文字もあまり多くない．特に日本語環境だと IPA フォントなどと併用して使われることが多い．僕自身は Computer Modern に代わる目的で作られた Latin Modern フォントと IPA フォントを使ってきたんだけど，最近数式環境だけ STIX フォントに切り替えた．そのメモ．

きっかけ
--------

クラス :math:`A`，:math:`B` について，:math:`A \subsetneq B` またはその逆か，:math:`A = B` か，互いに比較不能，つまり :math:`A \nsubseteq B \land B \nsubseteq A` かの何の関係であるかは重要になることが多い．ところで，比較不能に関してだけ2項演算ではないわけだけど，これを他と合わせて書く表記はないのだろうかというと，一応あって「⫓」を否定する記号が使われる．「⫓」の意味としては部分集合であり上位集合であるという意味になっていて，それを否定するのでそのどちらでもない，つまり片方だけに含まれる要素がそれぞれ存在するということになる．ただこの記号，LaTeX の標準パッケージ中ではサポートされていない．ところで，この記号は Unicode では U+2AD3 の文字としてサポートされていて，この文章でも Unicode 文字をそのまま使っている．

まあ，とてつもなくこの文字が出したいというわけではないんだけど，どうせだったので Unicode 対応フォントを使えないのかなということで探して見つけたのが，STIX フォントになる．それまでは Latin Modern フォントと :math:`\llbracket`，:math:`\rrbracket` を出すために ``stmaryrd`` というパッケージを合わせて使ってたんだけど，これも Unicode に含まれてるので STIX フォントに切り替えれば書かずにすむようになる．``stmaryrd`` はそもそも独自のフォントを提供してたのだけど，bold font に対応してないなどの問題点もあった．STIX フォントに切り替えればそういう問題も解決するし，フォント管理も一括でできるようになりそうという感じ．

STIX フォントと Latin Modern の違いは以下の感じ [#license-notice]_:

Latin Modern フォント
  .. image:: {attach}latex-with-stix-font/latin-modern-sample.png
    :alt: Latin Modern フォントによる数式サンプル
    :align: center

STIX フォント
  .. image:: {attach}latex-with-stix-font/stix-sample.png
    :alt: STIX フォントによる数式サンプル
    :align: center

ちょっと，STIX フォントの方が太めになってるのと，不格好な部分はあるけど，許容範囲ではあるかなという感じ．とりあえず，今のところ使ってて不便に感じる点はないけど，将来的には差し戻す可能性もあるかも．それを試すためのお試し使用みたいな面もある．

STIX フォントの導入
-------------------

普段僕は ``upLaTeX`` と ``dvipdfmx`` を使ってる．その場合の STIX フォントの導入方法を書いておく．といってもそれほど難しくない．CTAN で STIX フォントの対応パッケージが提供されてるので，それをインストールして使うだけ．STIX フォントは，Type1 フォントと OpenType フォントどちらも提供されてて，好きな方使えばいい．Type1 フォントの方は ``stix2-type1``，OpenType フォントの方は ``stix2-otf`` という名前で提供されてる．ここでは，Type1 フォントの方を使っていく．やることは，

1. まず，``stix2-type1`` パッケージをインストールする::

      tlmgr install stix2-type1

2. ``\usepackage{stix2}`` する

こんだけ．これをやると全てのフォントで STIX フォントが使われるようになるけど，数式だけに適用したかったら，

.. code-block:: latex

  \usepackage[notext]{stix2}

とする．これでとりあえず大丈夫．

通常使えない記号を使う
----------------------

``stix2`` パッケージでは，フォントの設定の他に数式記号コマンドもいくつか提供してくれてる．詳しくは，`ユーザマニュアル <http://mirrors.ctan.org/fonts/stix2-type1/stix2.pdf>`_ の Math Symbols の節を読むと良い．例えば

* ⫓ : ``\subsup``
* ⟦ : ``\lBrack``

みたいな感じ．既に定義されてる記号は，そっちが優先されるようになってるので，そこんとこも調整されてる．後デフォルトで ``textcomp`` パッケージを読むようになってて，それが嫌なら ``notextcomp`` オプションをつけるといい．

まとめ
------

というわけで，Latin Modern + IPA + stmaryrd から文書は Latin Modern + IPA，数式は STIX みたいな構成に変えて最近は LaTeX を書いてる．そのうち文書の方も STIX + IPA にするかもという感じ．STIX よりこっちの方がおすすめみたいなのあれば教えて欲しいかも．というわけで，今回は以上．

.. [#license-notice] Latin Modern は `The GUST Font License (GFL) <http://www.gust.org.pl/projects/e-foundry/licenses/GUST-FONT-LICENSE.txt/view>`_ ，STIX は `The SIL Open Font License <https://github.com/stipub/stixfonts/blob/master/docs/STIX_2.0.2_release_notes.txt>`_ で提供されている．
