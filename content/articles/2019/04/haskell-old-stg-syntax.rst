STG Version 2.5 の動作
======================

:date: 2019-04-19 22:00
:tags: Haskell, 抽象機械, GHC
:category: プログラミング言語

先日 STG の動作について説明する機会があったんだが，ちゃんと説明できなかった．で，ちょっとこれじゃまずいって思って，色々読み直してる．で，いざっていう時のためにまとめとくかみたいな気分になっており，この記事が錬成された．なお，今の GHC の STG では色々変わってるので通用しないとこも多い．そこは注意．

この記事での STG について
-------------------------

この記事での STG は， Spineless Tagless G-machine Version 2.5 の事を指している．内容としては，SPJ が出した 1992年 JFP の

  Jones, S. L. P. (1992). Implementing lazy functional languages on stock hardware: the Spineless Tagless G-machine. Journal of Functional Programming, 2(02), 127–202. https://doi.org/10.1017/S0956796800000319

を基にしている． Microsoft Research のページから PDF は見れる: https://www.microsoft.com/en-us/research/publication/implementing-lazy-functional-languages-on-stock-hardware-the-spineless-tagless-g-machine/

STG の名前は，

1. G-machine (graph reduction machine): Johnsson, T. (1987). Compiling Lazy Functional Languages: An introduction. PhD thesis, Chalmers University.
2. Spineless G-machine: Burn, G. L., Peyton Jones, S. L., & Robson, J. D. (1988). The spineless G-machine. In Proceedings of the 1988 ACM conference on LISP and functional programming - LFP ’88 (pp. 244–258). New York, New York, USA: ACM Press. https://doi.org/10.1145/62678.62717
3. Spineless Tagless G-machine

みたいな変遷をたどっている．今となっては， pointer tagging が採用されたため tagless というワードは通用しなくなったり，実行モデルが変わったりという感じだが [#spineless-is-still]_ ，今でも STG という名前が使われている．まあ大枠は G-machine の拡張なので，いけるでしょ [#shared-term-graph]_．

てなわけで， STG Version 2.5 について見ていこうと思う．

STG の概要
----------

Haskell は正直 ``case`` であっても評価が起きなかったり，式中でヒープ割り当てが発生して位置を特定しにくいとか，正直コードの動作が追えたものじゃない [#haskell-optimizing-community]_ ． STG は Haskell の文法に寄せながらも動作が分かりやすいようになっている型無しの言語で，実際の構文はのちに述べるが，主要な構文とその操作的意味論が大体以下のように対応する:

====================== ==============
構文                    操作的意味論
====================== ==============
関数適用                 末尾呼び出し
let 式                  ヒープ割り当て
case 式                 評価
コンストラクタへの適用     継続の返還
====================== ==============


まとめ
------

まとめたかったのは主に， Part III の具体的な実装の話だったんだが力尽きた．てことで今日はこれで．続きは書くかもしれないし，書かないかもしれない．

.. [#spineless-is-still] spineless は僕の認識が正しければ生きてるはずだが，G-machine の実装では spineless は普通だったみたいな話があり， spineless もそこまで気にする用語ではないって事ですね．
.. [#shared-term-graph] なお，元々 shared term graph という二重の意味が込められてたっぽい: https://gitlab.haskell.org/ghc/ghc/blob/ghc-8.6.4-release/compiler/stgSyn/StgSyn.hs#L4 ．こっちの言い方は今でも通用しそうだが， STG machine はこっちの言い方だと shared term graph machine という感じになりそうで， reduction が抜けてるのはなんかあれですね．
.. [#haskell-optimizing-community] Haskell 界には， Haskell 最適化委員会なる組織があるらしく，その人たちは Haskell プログラムでもそういうのを見通す力を持ってるらしい．
