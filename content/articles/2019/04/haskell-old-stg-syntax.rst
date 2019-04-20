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

てなわけで， STG Version 2.5 について見ていこうと思う．お気持ちの部分はかなりすっ飛ばすので，もっと詳しく知りたいとかちゃんとした情報が知りたい人は上の論文を読んでくれって感じ．今の STG に受け継がれてるとこは多いというか，多分上のやつ読まないとその後の話の大部分分からない気がするので，読んでみるのはオススメ．

STG の概要
----------

GHC では，

1. Haskell: これはみなさんご存知の通りのやつ
2. Core: 複雑な構文 (型クラスとかも) の脱糖，型の明記などが行われたやつ
3. STG: 抽象機械，大体の実行時の動作を表す表現
4. C--: C のサブセット
5. バックエンド: LLVM / アセンブリなど

みたいなフローでコンパイルが行われていて，大体は Core-to-Core で最適化が行われて， STG として吐き出された後操作的意味論に合うように C-- が吐き出される，いわばちょっと高尚な低レベル動作から離れた位置の言語から低レベル動作がある程度考慮された言語への橋渡しみたいな位置にいるのが， STG になる．

Haskell は正直 ``case`` であっても評価が起きなかったり，式中でヒープ割り当てが発生して位置を特定しにくいとか，正直コードの動作が追えたものじゃない [#haskell-optimizing-community]_ ． STG は Haskell の文法に寄せながらも動作が分かりやすいようになっている型無しの言語で，実際の構文はのちに述べるが，主要な構文とその操作的意味論が大体以下のように対応する:

====================== ==============
構文                    操作的意味論
====================== ==============
関数適用                 末尾呼び出し
let 式                  ヒープ割り当て
case 式                 評価
コンストラクタへの適用     継続への復帰
====================== ==============

コンストラクタへの適用が継続への復帰に対応するというのは分かる人には分かると思うけど，分からない人は何を言ってるか分からないと思う．これについては後述するので，今は気にしなくていい．この機能が tagless の由来だったりする．後の対応関係はいいと思う．通常の Haskell だとここまでうまく対応しないけど， STG は対応させるため構文に色々制約を設けてる．

STG の構文
----------

実際の構文は以下の感じ:

基本要素
  .. math::

    \begin{array}{ll}
    \mathit{vars} \mathrel{::=} \{\mathit{var}_1, \ldots, \mathit{var}_n\} & (n \geq 0) \\
    \mathit{var} \in \mathit{VAR} & (\text{\(\mathit{VAR}\) は変数集合}) \\
    \\
    \mathit{atoms} \mathrel{::=} \{\mathit{atom}_1, \ldots, \mathit{atom}_n\} & (n \geq 0) \\
    \mathit{atom}  \mathrel{::=} \mathit{var} \mid \mathit{literal} \\
    \\
    \mathit{literal} \in \mathit{LIT} & (\text{\(\mathit{LIT}\) はリテラル集合}) \\
    \mathit{prim} \in \mathit{PRIM} &(\text{\(\mathit{PRIM}\) はプリミティブ演算子の集合}) \\
    \mathit{constr} \in \mathit{CON} &(\text{\(\mathit{CON}\) はコンストラクタの集合})
    \end{array}

  基本要素は以上のやつになる．リテラルは， unboxed な値を想定してて， :math:`\mathit{LIT} = \{\text{\tt 0\#}, \text{\tt 1\#}, \ldots\}` みたいなのが書ける．また，プリミティブ演算子も同じく， :math:`\mathit{PRIM} = \{\text{\tt +\#}, \text{\tt *\#}, \ldots\}` みたいなのが書ける．変数とコンストラクタは Haskell と同じ習慣に従ってて，アルファベットの場合変数は先頭小文字，コンストラクタは大文字で始まる．ただ，ここら辺は本質情報じゃなくてもちろん取り換え可能．

ラムダ式
  .. math::

    \begin{array}{l}
    \mathit{lf} \mathrel{::=} \mathit{vars}_f \mathrel{\text{\tt \textbackslash} \pi} \mathit{vars}_a \mathrel{\text{\tt ->}} \mathit{expr} \\
    \\
    \pi \mathrel{::=} \mathtt{u} \mid \mathtt{n}
    \end{array}

  この式はクロージャを表し， :math:`\mathit{vars}_f` が自由変数， :math:`\mathit{vars}_a` がクロージャの引数になる． :math:`\pi` は更新フラグで，更新できるかどうかを表す． ``u`` が更新可能， ``n`` が更新不可能を表し， ``u`` の場合要はサンクを表す． :math:`\mathit{expr}` はまだ説明してないが主要な構文と :math:`atom` ぐらいが書ける．この後に書いてあるので，気になったら適当にフライングしてくれ．

束縛
  .. math::
    \begin{array}{ll}
    \mathit{program} \mathrel{::=} \mathit{binds} \\
    \\
    \mathit{binds} \mathrel{::=} \mathit{var}_1 = \mathit{lf}_1 \mathpunct{;} \cdots \mathpunct{;} \mathit{var}_n = \mathit{lf}_n &(n \geq 1)
    \end{array}

  束縛は， let 式で使われる他， STG のプログラムも束縛で表現される． Haskell と違って， STG では束縛でパターンマッチとかできないし，クロージャしか束縛できない．なお， STG にはもう 1 つ束縛の仕方があって， case 式を使えば評価結果を束縛できる．なので，この束縛はヒープ割り当て用， case は式の評価値束縛用みたいな感じ．束縛はクロージャしかできないので，プリミティブな値を束縛したい時とかも case を使う必要がある．

let (rec) 式
  .. math::

    \begin{array}{rl}
    \mathit{letexpr}
    \mathrel{::=}& \text{\tt let} \,\mathit{binds}\,\text{\tt in}\, \mathit{expr} \\
    \mid& \text{\tt letrec} \,\mathit{binds}\,\text{\tt in}\, \mathit{expr}
    \end{array}

  まあこれはいいでしょ． Haskell では let 式中で再帰的な変数が書け，コンパイル時に静的に再帰的かどうか解析されるが， STG では再帰的に書けるかどうかが構文レベルで区別されている．もちろん， let は素直に実装できるが， letrec は循環参照を含む場合があるのでちょっと工夫が必要．後，今回は説明しないが letrec は無限ループチェックのためブラックホールという機能も搭載されるので，重いみたいなんもある．

case 式
  .. math::

    \begin{array}{l}
    \mathit{caseexpr} \mathrel{::=} \text{\tt case} \, \mathit{expr} \, \text{\tt of} \, \mathit{alts} \\
    \\
    \begin{array}{rll}
    \mathit{alts}
    \mathrel{::=}& \mathit{aalt}_1 \mathpunct{;} \cdots \mathpunct{;} \mathit{aalt}_n \mathpunct{;} \mathit{default} &(n \geq 0) \\
    \mid& \mathit{palt}_1 \mathpunct{;} \cdots \mathpunct{;} \mathit{palt}_n \mathpunct{;} \mathit{default} &(n \geq 0)
    \end{array} \\
    \\
    \begin{array}{rl}
    \mathit{aalt} \mathrel{::=}& \mathit{constr}\,\mathit{vars} \mathrel{\text{\tt ->}} \mathit{expr} \\
    \mathit{palt} \mathrel{::=}& \mathit{literal} \mathrel{\text{\tt ->}} \mathit{expr} \\
    \mathit{default} \mathrel{::=}& \mathit{var} \mathrel{\text{\tt ->}} \mathit{expr} \\
    \mid& \text{\tt default} \mathrel{\text{\tt ->}} \mathit{expr}
    \end{array}
    \end{array}

  case 式は，まず一層しかパターンマッチできなくて，しかもリテラルかコンストラクタかで分かれてる (これは当たり前といえばそうか)． でいずれにもマッチしなかった場合のデフォルトで評価値を捨てるか，束縛するかを選べるみたいな感じ．これもまあいいですね．

関数適用
  .. math::

    \begin{array}{rl}
    \mathit{appexpr}
    \mathrel{::=}& \mathit{var}\, \mathit{atoms} \\
    \mid& \mathit{constr}\, \mathit{atoms} \\
    \mid& \mathit{prim}\, \mathit{atoms}
    \end{array}

  関数適用は適用する対象によって色々分かれてる．動作も違ったりするが，それについては後ほど．あともう一つの特徴として， Haskell と違って引数は必ず事前にヒープ割り当てしてある変数 [#all-variables-were-heap-allocated]_ かリテラルだけ．

式
  .. math::

    \begin{array}{l}
    \mathit{expr}
    \mathrel{::=} \mathit{letexpr}
    \mid \mathit{caseexpr}
    \mid \mathit{appexpr}
    \mid \mathit{literal}
    \end{array}

  式は単純に今まで出てきた主要な式 + リテラルが書ける感じ．これは特にいうことないっすね．

気持ち的には特に最適化を考慮しなければ， Haskell と STG は以下の感じで対応する [#example-by-paper]_ ．

Haskell::

  map :: (a -> b) -> [a] -> [b]
  map f []     = []
  map f (x:xs) = f x : map f xs

STG::

  map = {} \n {f, xs} ->
    case xs of
      Nil {}       -> Nil {}
      Cons {y, ys} ->
        let fy   = {f, y} \u {} -> f {y}
            mfys = {f, ys} \u {} -> map {f, ys}
        in Cons {fy, mfys}

``Nil`` と ``Cons`` が具体的にどう表現されるかは後ほど示すので，今は ``[]`` と ``:`` に対応するものとだけ思ってもらえれば良い． ``map = \f xs -> case xs of ...`` に注意すると，最初の方は良いだろう． ``map`` はラムダそのままなので既に WHNF でありサンクにする要素はないので ``n`` (更新不要) が指定される．逆に ``fy`` と ``mfys`` はサンクになっていて，それぞれクロージャとしてヒープ割り当てが行われる．この割り当ては，適用の際引数が変数かリテラルでないとだめという制約に忠実に従うとこう書くしかなくて， STG は構文レベルで実行動作と対応できるようになっている．

後論文でもう 1 つ例が挙げられていたので，そちらも紹介しておく．

Haskell::

  map1 :: (a -> b) -> [a] -> [b]
  map1 f = mf
    where
      mf []     = []
      mf (x:xs) = f x : mf xs

STG::

  map1 = {} \n {f} ->
    letrec
      mf = {f, mf} \n {xs} ->
        case xs of
          Nil {}       -> Nil {}
          Cons {y, ys} ->
            let fy   = {f, y} \u {} -> f {y}
                mfys = {mf, ys} \u {} -> mf {ys}
            in Cons {fy, mfys}
    in mf

この例は重要な例ではあるんだけど，その重要さは後ほど説明する．とりあえずここでは， STG の構文に慣れてもらえれば良い．分かっている人は， STG のクロージャは自由変数と引数両方持てるよと認識してもらえば良い．今回は ``mf`` がその例となっている．

STG の意味論
------------

準備
::::

STG の意味論は表示的にはクロージャの自由変数とか更新フラグとか全部無視して Haskell と同じ感じの意味を持たせることができる．ただ抽象機械なので重要なのは操作的な方だよねってことで，操作的意味論をまとめておく．

操作的意味論は，状態機械で定義されていて，遷移は評価ステップになっている．基本的な用語として，

アドレス
  ヒープのアドレス．表現はなんでも良いが，通常の非負整数と思っておいてくれ．

値
  以下の 2 種類:

  * :math:`\mathbf{Addr}\,a` : アドレス
  * :math:`\mathbf{Int}\,n` : プリミティブの整数

  なお，プリミティブな値は正直増やそうと思えばいくらでも増やせるが，定義を書く上ではめんどいので，整数のみを扱う．

で使っていくのでよろしく．で，状態機械の状態は以下の要素の組として定義されている:

引数スタック ( :math:`\mathit{as}` )
  値の列．関数の引数として使用されるやつらが入ってる．

返り値スタック ( :math:`\mathit{rs}` )
  継続の列．これが tagless の所以なんだが，返り値スタックが使用されるのは case 式で， case はまず評価が終わった後の分岐 (パターンマッチ) を継続の形で返り値スタックに入れておき，評価値が返り値スタックの分岐から継続を選びそこを実行するみたいな感じになっている．実際の動きは，この後の操作的意味論を見ながら話す．

更新スタック ( :math:`\mathit{us}` )
  サンクを評価する際に，現在のスタック情報を退避させておくためのフレームスタックで，退避させた引数スタックと返り値スタック，更新しているクロージャのアドレスの列が入っている．

ヒープ ( :math:`h` )
  アドレスとクロージャの対応が入ってる．

グローバル環境 ( :math:`\sigma` )
  トップレベルで束縛されたクロージャのアドレスが入ってる．

命令
  以下の 4 種類の命令:

  * :math:`\mathbf{Eval}\,e\,\rho` : STG の式 :math:`e` を環境 :math:`\rho` で評価する．
  * :math:`\mathbf{Enter}\,a` : アドレス :math:`a` にあるクロージャに，引数スタックに積まれてるものを引数として適用する．
  * :math:`\mathbf{ReturnCon}\,c\,\mathit{ws}` : コンストラクタ :math:`c` とその適用された値 :math:`ws` から，返り値スタックにある継続を実行する．
  * :math:`\mathbf{ReturnInt}\,n` : プリミティブ整数 :math:`n` から，返り値スタックにある継続を実行する．

  なお， :math:`\mathbf{ReturnInt}` は :math:`\mathbf{ReturnCon}` のプリミティブ向け特殊版と考えれば良い．整数は引数無しのコンストラクタと大体同じ．

カッコ内に書いてあるのはメタ変数．以降はこの系統のメタ変数を使っていく．では，実際の意味論を見ていく．

初期状態
::::::::

まず， STG のプログラム

.. math::

  \begin{array}{c}
  g_1 = \mathit{vs}_1 \mathrel{\text{\tt \textbackslash}\pi_1} \mathit{xs}_1 \mathrel{\text{\tt ->}} e_1 \\
  \vdots \\
  g_n = \mathit{vs}_n \mathrel{\text{\tt \textbackslash}\pi_n} \mathit{xs}_n \mathrel{\text{\tt ->}} e_n
  \end{array}

に対して初期状態は次のようになる:

.. math::

  \begin{array}{l}
  \langle \mathbf{Eval}\,(\text{\tt main \{\}})\,[]\rangle(
    \mathit{as}_\mathit{in},
    \mathit{rs}_\mathit{in},
    \mathit{us}_\mathit{in},
    h_\mathit{in},
    \sigma
  ) \\
  \begin{array}{ll}
    \mathit{as}_\mathit{in} &= [] \\
    \mathit{rs}_\mathit{in} &= [] \\
    \mathit{us}_\mathit{in} &= [] \\
    h_\mathit{in} &= \left[\begin{matrix}
      a_1 \mapsto \langle\mathit{vs}_1 \mathrel{\text{\tt \textbackslash}\pi_1} \mathit{xs}_1 \mathrel{\text{\tt ->}} e_1, \sigma(\mathit{vs}_1)\rangle \\
      \vdots \\
      a_n \mapsto \langle\mathit{vs}_n \mathrel{\text{\tt \textbackslash}\pi_n} \mathit{xs}_n \mathrel{\text{\tt ->}} e_n, \sigma(\mathit{vs}_n)\rangle
    \end{matrix}\right] \\
    \sigma_\mathit{in} &= \left[\begin{matrix}
      g_1 \mapsto \mathbf{Addr}\,a_1 \\
      \vdots \\
      g_n \mapsto \mathbf{Addr}\,a_n
    \end{matrix}\right]
  \end{array}
  \end{array}

最初はトップレベルの束縛の中に ``main`` 関数がある前提でそこから評価を始める．ここは本質じゃないので， :math:`g_1` とかから始めてもいい．トップレベルの束縛は， letrec 式に相当するので，自由変数の部分にトップレベルから取ってきたものをあてがって，ヒープに入れておく．

この状態から状態遷移を始めていく．

関数適用
::::::::



まとめ
------

まとめたかったのは主に， Part III の具体的な実装の話だったんだが力尽きた．てことで今日はこれで．続きは書くかもしれないし，書かないかもしれない．

機会があればもうちょっと正確を期して，どっかに上げるかもしれない．

.. [#spineless-is-still] spineless は僕の認識が正しければ生きてるはずだが，G-machine の実装では spineless は普通だったみたいな話があり， spineless もそこまで気にする用語ではないって事ですね．
.. [#shared-term-graph] なお，元々 shared term graph という二重の意味が込められてたっぽい: https://gitlab.haskell.org/ghc/ghc/blob/ghc-8.6.4-release/compiler/stgSyn/StgSyn.hs#L4 ．こっちの言い方は今でも通用しそうだが， STG machine はこっちの言い方だと shared term graph machine という感じになりそうで， reduction が抜けてるのはなんかあれですね．
.. [#haskell-optimizing-community] Haskell 界には， Haskell 最適化委員会なる組織があるらしく，その人たちは Haskell プログラムでもそういうのを見通す力を持ってるらしい．
.. [#all-variables-were-heap-allocated] 厳密には全てがヒープ割り当てされてるとは限らなくて， case での束縛はスタックに積まれるみたいなこともある．
.. [#example-by-paper] 例は論文中から持ってきたもの．
