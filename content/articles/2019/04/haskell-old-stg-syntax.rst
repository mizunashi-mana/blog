STG Version 2.5 の動作
======================

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

みたいなフローでコンパイルが行われていて，大体は Core-to-Core で最適化が行われて， STG として吐き出された後操作的意味論に合うように C-- が吐き出される，いわば低レベル動作から離れた位置の高尚な言語から，低レベル動作がある程度考慮された言語への橋渡しみたいな位置にいるのが， STG になる．

Haskell は ``case`` であっても評価が起きなかったり，式中でヒープ割り当てが発生して位置を特定しにくいとか，正直コードの動作が追えたものじゃない [#haskell-optimizing-community]_ ． STG は Haskell の文法に寄せながらも動作が分かりやすいようになっている型無しの言語で，主要な構文とその操作的意味論が大体以下のように対応する:

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

  この式はクロージャを表し， :math:`\mathit{vars}_f` が自由変数， :math:`\mathit{vars}_a` がクロージャの引数になる． :math:`\pi` は更新フラグで，更新できるかどうかを表す． ``u`` が更新可能， ``n`` が更新不要を表し， ``u`` の場合要はサンクを表す．後更新フラグは以下のように決める:

  * 以下に該当するものは ``n`` をセットする:

    * 引数が 1 以上のクロージャ: ``{f} \n {x} -> case 1# of v -> f {v, x}``
    * 中身がコンストラクタ適用となっているクロージャ: ``{x, l} \n {} -> Cons {x, l}``
    * (オプショナル) 部分適用
    * (オプショナル) 一度しか使われないことが分かっているクロージャ

  * それ以外の場合は， ``u`` をセットする．

  これは本質的な制約じゃなくて，更新フラグを全部 ``u`` から始めてもいい．ただ， STG は抽象機械を想定してるので，最初から更新不要と分かってるとこには ``n`` を書いておいた方が都合がいい．なので，後ろに載せた意味論はこの制約を守ってる前提で書かれてる．なお，オプショナルになっている制約は，実はやらなくても意味論は機能する．特に，クロージャが一度しか使われないかを完全に解析することは困難なので，一度しか使われないクロージャでも ``u`` がセットされることはある．

  :math:`\mathit{expr}` はまだ説明してないが主要な構文と :math:`atom` ぐらいが書ける．この後に書いてあるので，気になったら適当にフライングしてくれ．

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

  まあこれはいいでしょ． Haskell では let 式中で再帰的な変数が書け，コンパイル時に静的に再帰的かどうか解析されるが， STG では再帰的に書けるかどうかが構文レベルで区別されている．もちろん， let は素直に実装できるが， letrec は循環参照を含む場合があるのでちょっと工夫が必要．後，今回は説明しないが letrec は無限ループチェックのためブラックホールという機能が搭載されるので，重いみたいなんもある．

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

  case 式は，まず一層しかパターンマッチできなくて，しかもリテラルかコンストラクタかで分かれてる (これは当たり前といえばそうか)． でいずれにもマッチしなかった場合のデフォルトで評価値を捨てるか，束縛するかを選べるみたいな感じ．

関数適用
  .. math::

    \begin{array}{rl}
    \mathit{appexpr}
    \mathrel{::=}& \mathit{var}\, \mathit{atoms} \\
    \mid& \mathit{constr}\, \mathit{atoms} \\
    \mid& \mathit{prim}\, \mathit{atoms}
    \end{array}

  関数適用は適用する対象によって色々分かれてる．動作も違ったりするが，それについては後ほど．あともう一つの特徴として， Haskell と違って，引数は必ず事前にヒープ割り当てしてある変数 [#all-variables-were-heap-allocated]_ かリテラルだけ．

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

``Nil`` と ``Cons`` は ``[]`` と ``:`` に対応する単なるコンストラクタを表す文字列と思ってもらって良い．こいつら自体に特に表現はない．それでどうやってパターンマッチを動作させるのかは，意味論を参照． ``map = \f xs -> case xs of ...`` に注意すると，最初の方は良いだろう． ``map`` はラムダそのままなので既に WHNF であり，サンクにする要素はないので ``n`` (更新不要) が指定される．逆に ``fy`` と ``mfys`` はサンクになっていて，それぞれクロージャとしてヒープ割り当てが行われる．この割り当ては，適用の際引数が変数かリテラルでないとだめという制約に忠実に従うとこう書くしかなくて， STG は構文レベルで実行動作と対応できるようになっている．

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

この例は重要な例ではあるんだけど，とりあえずここでは， STG の構文に慣れてもらえれば良い．分かっている人は， STG のクロージャは自由変数と引数両方持てるよと認識してもらえば良い．今回は ``mf`` がその例となっている．

STG の意味論
------------

STG の意味論は表示的にはクロージャの自由変数とか更新フラグとか全部無視して Haskell と同じ感じの意味を持たせることができる．ただ抽象機械なので重要なのは操作的な方だよねってことで，操作的意味論をまとめておく．

準備
::::

操作的意味論は，状態機械で定義されていて，遷移は評価ステップになっている．基本的な用語として，

アドレス
  ヒープのアドレス．表現はなんでも良いが，通常の非負整数と思っておいてくれ．

値
  以下の 2 種類:

  * :math:`\mathbf{Addr}\,a` : アドレス
  * :math:`\mathbf{Int}\,n` : プリミティブの整数

  なお，プリミティブな値は増やそうと思えばいくらでも増やせるが，定義を書く上ではめんどいので，整数のみを扱う．

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
  トップレベルで束縛された変数とクロージャのアドレスの対応が入ってる．

命令
  以下の 4 種類の命令:

  * :math:`\mathbf{Eval}\,e\,\rho` : STG の式 :math:`e` を環境 :math:`\rho` で評価する．
  * :math:`\mathbf{Enter}\,a` : アドレス :math:`a` にあるクロージャに，引数スタックに積まれてるものを引数として適用する．
  * :math:`\mathbf{ReturnCon}\,c\,\mathit{ws}` : コンストラクタ :math:`c` とその適用された値 :math:`ws` から，返り値スタックにある継続を実行する．
  * :math:`\mathbf{ReturnInt}\,n` : プリミティブ整数 :math:`n` から，返り値スタックにある継続を実行する．

  なお， :math:`\mathbf{ReturnInt}` は :math:`\mathbf{ReturnCon}` のプリミティブ向け特殊版と考えれば良い．整数は引数無しのコンストラクタと大体同じ．

カッコ内に書いてあるのはメタ変数．以降はこの系統のメタ変数を使っていく．それから以下の補助関数を用意しておく:

.. math::

  \begin{array}{c}
  \mathit{val}(\langle \rho, \sigma\rangle, x) = \left\{\begin{array}{ll}
    \mathbf{Int}\,n &(\text{\(x = n\) は整数}) \\
    v &(\rho(x) = v) \\
    \sigma(x) &(\text{otherwise})
  \end{array}\right. \\
  \mathit{vals}(\langle \rho, \sigma\rangle, \{x_1, \ldots, x_n\}) =
  [\mathit{val}(\langle \rho, \sigma\rangle, x_1), \ldots, \mathit{val}(\langle \rho, \sigma\rangle, x_n)]
  \end{array}

この関数はまずローカルスコープで変数を探して，なかったらグローバルスコープで探し，その変数に対応するクロージャのアドレスを返す．では，実際の意味論を見ていく．

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
      a_1 \mapsto \langle\mathit{vs}_1 \mathrel{\text{\tt \textbackslash}\pi_1} \mathit{xs}_1 \mathrel{\text{\tt ->}} e_1, \mathit{vals}(\langle [], \sigma\rangle, \mathit{vs}_1)\rangle \\
      \vdots \\
      a_n \mapsto \langle\mathit{vs}_n \mathrel{\text{\tt \textbackslash}\pi_n} \mathit{xs}_n \mathrel{\text{\tt ->}} e_n, \mathit{vals}(\langle [], \sigma\rangle, \mathit{vs}_n)\rangle
    \end{matrix}\right] \\
    \sigma &= [g_1 \mapsto \mathbf{Addr}\,a_1, \ldots, g_n \mapsto  \mathbf{Addr}\,a_n]
  \end{array}
  \end{array}

最初はトップレベルの束縛の中に ``main`` 関数がある前提でそこから評価を始める．ここは本質じゃないので， :math:`g_1` とかから始めてもいい．トップレベルの束縛は， letrec 式に相当するので，自由変数の部分にトップレベルから取ってきたものをあてがって，ヒープに入れておく．

この状態から状態遷移を始めていく．

評価
::::

まず， :math:`\mathbf{Eval}` 命令の遷移から見ていく．基本的に STG のそれぞれの式に対して，それに合う遷移をしていく．その定義は，以下のようになる:

let (rec) 式
  .. math::

    \begin{array}{c}
    \langle \mathbf{Eval}\,\left(\begin{array}{lc}
      \text{\tt let}
      &x_1 = \mathit{vs}_1 \mathrel{\text{\tt \textbackslash}\pi_1} \mathit{xs}_1 \mathrel{\text{\tt ->}} e_1 \\
      &\vdots \\
      &x_n = \mathit{vs}_n \mathrel{\text{\tt \textbackslash}\pi_n} \mathit{xs}_n \mathrel{\text{\tt ->}} e_n \\
      \text{\tt in}\,e
      \end{array}\right)\,\rho\rangle(
      \mathit{as},
      \mathit{rs},
      \mathit{us},
      h,
      \sigma
    )
    \Rightarrow
    \langle \mathbf{Eval}\,e\,\rho'\rangle(
      \mathit{as},
      \mathit{rs},
      \mathit{us},
      h',
      \sigma
    ) \\
    (\rho' = \rho\left[\begin{array}{c}
      x_1 \mapsto \mathbf{Addr}\,a_1 \\
      \vdots \\
      x_n \mapsto \mathbf{Addr}\,a_n
      \end{array}\right]
    , h' = h\left[\begin{array}{c}
      a_1 \mapsto \langle \mathit{vs}_1 \mathrel{\text{\tt \textbackslash}\pi_1} \mathit{xs}_1 \mathrel{\text{\tt ->}} e_1, \mathit{vals}(\langle \rho_{\mathit{rhs}}, []\rangle, \mathit{vs}_1)\rangle \\
      \vdots \\
      a_n \mapsto \langle \mathit{vs}_n \mathrel{\text{\tt \textbackslash}\pi_n} \mathit{xs}_n \mathrel{\text{\tt ->}} e_n, \mathit{vals}(\langle \rho_{\mathit{rhs}}, []\rangle, \mathit{vs}_n)\rangle
      \end{array}\right]
    , \rho_\mathit{rhs} = \rho
    )
    \end{array}

  letrec 式の場合は， :math:`\rho_\mathit{rhs} = \rho'` とする．この遷移は，単純に let で指定されたローカルのクロージャをヒープに確保し，そのアドレスを変数に結びつけるだけ． let と letrec の違いは作るクロージャでキャプチャするアドレスの違いで， let の場合は前の環境から， letrec の場合は今回確保したアドレスも含めてキャプチャする．

case 式
  .. math::

    \langle \mathbf{Eval}\,(\text{\tt case}\,e\,\text{\tt of}\,\mathit{alts})\,\rho\rangle(
      \mathit{as},
      \mathit{rs},
      \mathit{us},
      h,
      \sigma
    )
    \Rightarrow
    \langle \mathbf{Eval}\,e\,\rho\rangle(
      \mathit{as},
      \langle\mathit{alts}, \rho\rangle\mathbin{:}\mathit{rs},
      \mathit{us},
      h,
      \sigma
    )

  この遷移では，分岐の継続を返り値スタックに積んだ後，対象の式の評価に移る．最終的に :math:`\mathbf{ReturnCon}` とか :math:`\mathbf{ReturnInt}` で帰ってきて，元の環境で継続に復帰する．

適用
  .. math::

    \begin{array}{c}
    \langle \mathbf{Eval}\,(f\,\mathit{xs})\,\rho\rangle(
      \mathit{as},
      \mathit{rs},
      \mathit{us},
      h,
      \sigma
    )
    \Rightarrow
    \langle \mathbf{Enter}\,a\rangle(
      \mathit{as'},
      \mathit{rs},
      \mathit{us},
      h,
      \sigma
    ) \\
    (\mathit{val}(\langle \rho, \sigma\rangle, f) = \mathbf{Addr}\,a, \mathit{as'} = \mathit{vals}(\langle \rho, \sigma\rangle, \mathit{xs}) \mathbin{++} \mathit{as}) \\
    \\
    \langle \mathbf{Eval}\,(v\,\{\})\,\rho\rangle(
      \mathit{as},
      \mathit{rs},
      \mathit{us},
      h,
      \sigma
    )
    \Rightarrow
    \langle \mathbf{ReturnInt}\,n\rangle(
      \mathit{as},
      \mathit{rs},
      \mathit{us},
      h,
      \sigma
    ) \\
    (\mathit{val}(\langle \rho, \sigma\rangle, v) = \mathbf{Int}\,n)
    \end{array}

  変数への適用の場合 2 種類あって，クロージャへ引数を適用する場合とプリミティブ整数の評価の場合．クロージャの場合変数には :math:`\mathbf{Addr}` が結びついていて，プリミティブ整数の場合 :math:`\mathbf{Int}` が結びついてる．クロージャの場合，引数を引数スタックに積み込んで適用に移る．プリミティブ整数の場合，そのまま継続への復帰に遷移する．

コンストラクタ適用
  .. math::

    \langle \mathbf{Eval}\,(c\,\mathit{xs})\,\rho\rangle(
      \mathit{as},
      \mathit{rs},
      \mathit{us},
      h,
      \sigma
    )
    \Rightarrow
    \langle \mathbf{ReturnCon}\,c\,\mathit{vals}(\langle \rho, \sigma\rangle, \mathit{xs})\rangle(
      \mathit{as},
      \mathit{rs},
      \mathit{us},
      h,
      \sigma
    )

  コンストラクタへの適用は，単純に適用された変数から値を持ってきて，継続へ復帰するだけ．

プリミティブ
  .. math::

    \begin{array}{c}
    \langle \mathbf{Eval}\,n\,\rho\rangle(
      \mathit{as},
      \mathit{rs},
      \mathit{us},
      h,
      \sigma
    )
    \Rightarrow
    \langle \mathbf{ReturnInt}\,n\rangle(
      \mathit{as},
      \mathit{rs},
      \mathit{us},
      h,
      \sigma
    ) \\
    \\
    \langle \mathbf{Eval}\,(\oplus\,\{x_1, x_2\})\,\rho\rangle(
      \mathit{as},
      \mathit{rs},
      \mathit{us},
      h,
      \sigma
    )
    \Rightarrow
    \langle \mathbf{ReturnInt}\,(i_1 \oplus i_2)\rangle(
      \mathit{as},
      \mathit{rs},
      \mathit{us},
      h,
      \sigma
    ) \\
    (\mathit{vals}(\langle \rho, \sigma\rangle, \{x_1, x_2\}) = [\mathbf{Int}\,i_1, \mathbf{Int}\,i_2])
    \end{array}

  プリミティブ整数やプリミティブ演算は，そのまま継続へ復帰するだけ． STG では必ず評価は case 式でのみ行われるので，プリミティブ演算の引数にサンクは入ってこないことに注意．サンクを入れたい場合， case でサンクを潰した後それを束縛して渡してやる必要がある．

適用
::::

次に， :math:`\mathbf{Enter}` 命令の遷移から見ていく． :math:`\mathbf{Enter}` 命令は，引数が充足してる場合は更新フラグを見て，いい感じに処理をする．その定義は，以下のようになる．

更新不要クロージャ
  .. math::

    \begin{array}{c}
    \langle\mathbf{Enter}\,a\rangle(
      \mathit{as},
      \mathit{rs},
      \mathit{us},
      h,
      \sigma
    )
    \Rightarrow
    \langle\mathbf{Eval}\,e\,\rho\rangle(
      \mathit{as'},
      \mathit{rs},
      \mathit{us},
      h,
      \sigma
    ) \\
    ( \mathit{ws}_a \mathbin{++} \mathit{as'} = \mathit{as}
    , |\mathit{ws}_a| = |\mathit{xs}|
    , \rho = [\mathit{vs} \mapsto \mathit{ws}_f, \mathit{xs} \mapsto \mathit{ws}_a]
    , h(a) = \langle\mathit{vs} \mathrel{\text{\tt \textbackslash n}} \mathit{xs} \mathrel{\text{\tt ->}} e, \mathit{ws}_f\rangle
    )
    \end{array}

  引数が充足してて更新不要なクロージャの場合，単なる関数適用を行う．環境はキャプチャしておいた自由変数と，引数の変数分を作って渡す．なお，論文中だとヒープからクロージャのアドレスを抜き去ってるように見えるんだが，大丈夫なんだろか．とりあえず，こっちでは修正しといた．ただ，表記法が定義されてないので，解釈違いかもしれん．

更新可能クロージャ
  .. math::

    \begin{array}{c}
    \langle\mathbf{Enter}\,a\rangle(
      \mathit{as},
      \mathit{rs},
      \mathit{us},
      h[a \mapsto \langle\mathit{vs} \mathrel{\text{\tt \textbackslash u}} \{\} \mathrel{\text{\tt ->}} e, \mathit{ws}_f\rangle],
      \sigma
    )
    \Rightarrow
    \langle\mathbf{Eval}\,e\,\rho\rangle(
      [],
      [],
      \langle\mathit{as}, \mathit{rs}, a\rangle \mathbin{:} \mathit{us},
      h,
      \sigma
    ) \\
    (\rho = [\mathit{vs} \mapsto \mathit{ws}_f])
    \end{array}

  更新が必要なクロージャ，つまりサンクは，古いクロージャのアドレスをヒープから消し，更新スタックに情報を退避させて，評価を行う．ところで，この時もし古いクロージャのアドレスにアクセスして評価するような STG プログラムがあれば，そのアドレスを消してしまっていると問題が起きる．ただ，更新中に更新してるクロージャに再度アクセスがあるということは，つまり無限ループが発生してるってことでもある．これは論文中ではブラックホールと呼ばれていて，実際の実行マシンではこれを検出し，エラーを出すようにしてる．

部分適用
  .. math::

    \begin{array}{c}
    \langle\mathbf{Enter}\,a\rangle(
      \mathit{as},
      [],
      \langle \mathit{as}_u, \mathit{rs}_u, a_u\rangle\mathbin{:}\mathit{us},
      h,
      \sigma
    )
    \Rightarrow
    \langle\mathbf{Enter}\,a\rangle(
      \mathit{as} \mathbin{++} \mathit{as}_u,
      \mathit{rs}_u,
      \mathit{us},
      h',
      \sigma
    ) \\
    \left(\begin{array}{c}
    h(a) = \langle \mathit{vs}\mathrel{\text{\tt \textbackslash n}}\mathit{xs}\mathrel{\text{\tt ->}} e, \mathit{ws}_f\rangle,
    |\mathit{as}| < |\mathit{xs}| \\
    \mathit{xs}_1 \mathbin{++} \mathit{xs}_2 = \mathit{xs},
    |\mathit{xs}_1| = |\mathit{as}|,
    h' = h[a_u \mapsto \langle (\mathit{vs} \mathbin{++} \mathit{xs}_1)\mathrel{\text{\tt \textbackslash n}}\mathit{xs}_2\mathrel{\text{\tt ->}} e, \mathit{ws}_f \mathbin{++} \mathit{as}\rangle]
    \end{array}\right)
    \end{array}

  引数スタックの要素の数が，クロージャに必要な引数の数に満たない時は，クロージャへの適用は部分適用扱いになる．部分適用の場合，部分適用を表すサンクの評価中なはずなので，サンクの内容を既に分かっている部分はキャプチャして，本来の引数の数を受け取る関数を表すクロージャに更新する．そして，サンクに適用されたはずの引数を退避させた更新スタックから取り出してきて，もう一度適用をやり直す．

  意味論上はこの規則で問題ないのだが，実装する時のことを考えると， :math:`(\mathit{vs} \mathbin{++} \mathit{xs}_1)\mathrel{\text{\tt \textbackslash n}}\mathit{xs}_2\mathrel{\text{\tt ->}} e` というクロージャを部分適用の際に作成するのはかなりめんどくさい．クロージャの中身はコンパイル時に通常生成されるわけだが，この場合動的に生成する必要が出てくる．または，全ての部分適用を想定して， :math:`e` 度に専用のクロージャコードをコンパイル時に生成するという方法も考えられる (普通はこちらが正攻法になる) ．ただ，もちろんそれはコンパイル時生成コードが大量に出てくるので避けたい．そこで，規則を以下のように変えることが考えられる:

  .. math::

    \begin{array}{c}
    \langle\mathbf{Enter}\,a\rangle(
      \mathit{as},
      [],
      \langle \mathit{as}_u, \mathit{rs}_u, a_u\rangle\mathbin{:}\mathit{us},
      h,
      \sigma
    )
    \Rightarrow
    \langle\mathbf{Enter}\,a\rangle(
      \mathit{as} \mathbin{++} \mathit{as}_u,
      \mathit{rs}_u,
      \mathit{us},
      h',
      \sigma
    ) \\
    \left(\begin{array}{c}
    h(a) = \langle \mathit{vs}\mathrel{\text{\tt \textbackslash n}}\mathit{xs}\mathrel{\text{\tt ->}} e, \mathit{ws}_f\rangle,
    |\mathit{as}| < |\mathit{xs}| \\
    \mathit{xs}_1 \mathbin{++} \mathit{xs}_2 = \mathit{xs},
    |\mathit{xs}_1| = |\mathit{as}|,
    h' = h[a_u \mapsto \langle (f \mathbin{:} \mathit{xs}_1)\mathrel{\text{\tt \textbackslash n}}\text{\tt \{\}}\mathrel{\text{\tt ->}} f\,\mathit{xs}_1, \mathbf{Addr}\,a \mathbin{:} \mathit{as}\rangle],
    \text{\(f\) は fresh な変数}
    \end{array}\right)
    \end{array}

  こうしておくと，部分適用用のクロージャを作っておくだけで，それを共有することができ，コード生成量もその手間も削減することができる．クロージャへのエントリが 1 回増えるが，そこら辺はより低レベルの最適化で消えることも期待できる．

継続への復帰
::::::::::::

最後に， :math:`\mathbf{ReturnCon}` 命令 / :math:`\mathbf{ReturnInt}` 命令の遷移から見ていく．継続への復帰は，両命令でやってることは同じなので，まず :math:`\mathbf{ReturnCon}` 命令だけ見ていく．その定義は，以下のようになる．

マッチする場合
  .. math::

    \begin{array}{c}
    \langle\mathbf{ReturnCon}\,c\,\mathit{ws}\rangle(
      \mathit{as},
      \langle \mathit{alts}, \rho\rangle\mathbin{:}\mathit{rs},
      \mathit{us},
      h,
      \sigma
    )
    \Rightarrow
    \langle\mathbf{Eval}\,e\,\rho[\mathit{vs} \mapsto \mathit{ws}]\rangle(
      \mathit{as},
      \mathit{rs},
      \mathit{us},
      h,
      \sigma
    ) \\
    (\mathit{alts} = \cdots\mathbin{;} c\,\mathit{vs}\mathrel{\text{\tt ->}}e\mathbin{;} \cdots)
    \end{array}

  継続のパターンマッチの中に該当するコンストラクタに対する継続があるときは，その継続に復帰する．

デフォルトケースの場合
  .. math::

    \begin{array}{c}
    \langle\mathbf{ReturnCon}\,c\,\mathit{ws}\rangle(
      \mathit{as},
      \langle \mathit{alts}, \rho\rangle\mathbin{:}\mathit{rs},
      \mathit{us},
      h,
      \sigma
    )
    \Rightarrow
    \langle\mathbf{Eval}\,e_d\,\rho\rangle(
      \mathit{as},
      \mathit{rs},
      \mathit{us},
      h,
      \sigma
    ) \\
    (\mathit{alts} = \left(\begin{array}{c}
      c_1\,\mathit{vs}_1\mathrel{\text{\tt ->}}e_1\mathbin{;} \\
      \vdots \\
      c_n\,\mathit{vs}_n\mathrel{\text{\tt ->}}e_n\mathbin{;} \\
      \text{\tt default}\mathrel{\text{\tt ->}}e_d
    \end{array}\right), \forall 1 \leq i \leq n\ldotp c \neq c_i)
    \end{array}

  コンストラクタにパターンマッチするものがなくて，デフォルトケースでの継続があるときは，その継続に復帰する．

デフォルトケースでの束縛
  .. math::

    \begin{array}{c}
    \langle\mathbf{ReturnCon}\,c\,\mathit{ws}\rangle(
      \mathit{as},
      \langle \mathit{alts}, \rho\rangle\mathbin{:}\mathit{rs},
      \mathit{us},
      h,
      \sigma
    )
    \Rightarrow
    \langle\mathbf{Eval}\,e_d\,\rho'\rangle(
      \mathit{as},
      \mathit{rs},
      \mathit{us},
      h',
      \sigma
    ) \\
    \left(\begin{array}{c}
    \mathit{alts} = \left(\begin{array}{c}
      c_1\,\mathit{vs}_1\mathrel{\text{\tt ->}}e_1\mathbin{;} \\
      \vdots \\
      c_n\,\mathit{vs}_n\mathrel{\text{\tt ->}}e_n\mathbin{;} \\
      v\mathrel{\text{\tt ->}}e_d
    \end{array}\right),
    \forall 1 \leq i \leq n\ldotp c \neq c_i \\
    \rho' = \rho[v \mapsto \mathbf{Addr}\,a],
    h' = h[a \mapsto \langle \mathit{vs}\mathrel{\text{\tt \textbackslash n}}\{\}\mathrel{\text{\tt ->}}c\,\mathit{vs}, \mathit{ws}\rangle] \\
    \text{\(\mathit{vs}\) は \(|\mathit{vs}| = |\mathit{ws}|\) を満たす fresh な変数列}
    \end{array}\right)
    \end{array}

  束縛のないデフォルトケースと同じように，コンストラクタにパターンマッチするものがなくて，束縛が必要なデフォルトケースでの継続があった場合，その継続に復帰する．ただ，結果を束縛する必要があるので，コンストラクタ適用に相当するクロージャを生成して，それを束縛変数に結びつける．

更新スタックからの復帰
  .. math::

    \begin{array}{c}
    \langle\mathbf{ReturnCon}\,c\,\mathit{ws}\rangle(
      [],
      [],
      \langle \mathit{as}_u, \mathit{rs}_u, a_u\rangle\mathbin{:}\mathit{us},
      h,
      \sigma
    )
    \Rightarrow
    \langle\mathbf{ReturnCon}\,c\,\mathit{ws}\rangle(
      \mathit{as}_u,
      \mathit{rs}_u,
      \mathit{us},
      h',
      \sigma
    ) \\
    (h' = h[a_u \mapsto \langle \mathit{vs}\mathrel{\text{\tt \textbackslash n}}\{\}\mathrel{\text{\tt ->}}c\,\mathit{vs}, \mathit{ws}\rangle],
    \text{\(\mathit{vs}\) は \(|\mathit{vs}| = |\mathit{ws}|\) を満たす fresh な変数列}
    )
    \end{array}

  そもそも返り値スタックを使い切ってしまった場合，更新スタックに要素があるなら，それはサンクを評価した結果出てきた評価値ということなので，サンクのあった部分に評価後の結果を表すクロージャを挿入して，元の評価に戻る．

:math:`\mathbf{ReturnInt}` の場合，デフォルトケースでの束縛時にヒープ割り当てを行わないで直接整数を束縛変数に結びつけるぐらいの違いしかない．

遷移例
::::::

では，意味論に則って，実際に STG のプログラムを動かしてみる．以下のプログラムを動かしてみる::

  main = {} \u {} ->
    let nil = {} \n {} -> Nil {}
        mapid = {} \u {} -> map1 {id}
    in case 1# of
      v ->
        let l = {v, nil} \n {} -> Cons {v, nil}
        in mapid {l}

  id = {} \n {x} -> x {}

  map1 = {} \n {f, xs} ->
    letrec mf = {f, mf} \n {ys} ->
              case ys {} of
                Nil {}       -> Nil {}
                Cons {z, zs} ->
                  let fz   = {f, z} \u {} -> f {z}
                      mfzs = {mf, zs} \u {} -> mf {zs}
                  in Cons {fz, mfzs}
    in mf {xs}

このプログラムを意味論に沿って動かすと，次の動作をする:

.. math::

  \begin{array}{l}
  \langle \mathbf{Eval}\,(\text{\tt main \{\}})\,[]\rangle(
    [],
    [],
    [],
    \left[\begin{array}{l}
      a_{\text{\tt main}} \mapsto \cdots \\
      a_{\text{\tt id}} \mapsto \cdots \\
      a_{\text{\tt map1}} \mapsto \cdots
    \end{array}\right],
    \sigma = \left[\begin{array}{l}
      \text{\tt main} \mapsto \mathbf{Addr}\,a_{\text{\tt main}} \\
      \text{\tt id} \mapsto \mathbf{Addr}\,a_{\text{\tt id}} \\
      \text{\tt map1} \mapsto \mathbf{Addr}\,a_{\text{\tt map1}}
    \end{array}\right]
  ) \\
  \Rightarrow
  \langle \mathbf{Enter}\,a_{\text{\tt main}}\rangle(
    [],
    [],
    [],
    \left[\begin{array}{l}
      a_{\text{\tt main}} \mapsto \cdots \\
      a_{\text{\tt id}} \mapsto \cdots \\
      a_{\text{\tt map1}} \mapsto \cdots
    \end{array}\right],
    \sigma
  ) \\
  \Rightarrow
  \langle \mathbf{Eval}\,(\text{\tt let nil =} \cdots)\,[]\rangle(
    [],
    [],
    \langle [], [], a_{\text{\tt main}}\rangle\mathbin{:}[],
    \left[\begin{array}{l}
      a_{\text{\tt id}} \mapsto \cdots \\
      a_{\text{\tt map1}} \mapsto \cdots
    \end{array}\right],
    \sigma
  ) \\
  \Rightarrow
  \langle \mathbf{Eval}\,(\text{\tt case 1\# of} \cdots)\,\left[\begin{array}{l}
    \text{\tt nil} \mapsto \mathbf{Addr}\,a_{\text{\tt nil}} \\
    \text{\tt mapid} \mapsto \mathbf{Addr}\,a_{\text{\tt mapid}}
  \end{array}\right]\rangle(
    [],
    [],
    \langle [], [], a_{\text{\tt main}}\rangle\mathbin{:}[],
    \left[\begin{array}{l}
      a_{\text{\tt id}} \mapsto \cdots \\
      a_{\text{\tt map1}} \mapsto \cdots \\
      a_{\text{\tt nil}} \mapsto \cdots \\
      a_{\text{\tt mapid}} \mapsto \cdots
    \end{array}\right],
    \sigma
  ) \\
  \Rightarrow
  \langle \mathbf{Eval}\,\text{\tt 1\#}\,[\cdots]\rangle(
    [],
    [\langle\text{\tt v -> }\cdots, \left[\begin{array}{l}
    \text{\tt nil} \mapsto \mathbf{Addr}\,a_{\text{\tt nil}} \\
    \text{\tt mapid} \mapsto \mathbf{Addr}\,a_{\text{\tt mapid}}
  \end{array}\right]\rangle],
    [\langle [], [], a_{\text{\tt main}}\rangle],
    \left[\begin{array}{l}
      a_{\text{\tt id}} \mapsto \cdots \\
      a_{\text{\tt map1}} \mapsto \cdots \\
      a_{\text{\tt nil}} \mapsto \cdots \\
      a_{\text{\tt mapid}} \mapsto \cdots
    \end{array}\right],
    \sigma
  ) \\
  \Rightarrow
  \langle \mathbf{ReturnInt}\,1\rangle(
    [],
    [\langle\text{\tt v -> }\cdots, \left[\begin{array}{l}
    \text{\tt nil} \mapsto \mathbf{Addr}\,a_{\text{\tt nil}} \\
    \text{\tt mapid} \mapsto \mathbf{Addr}\,a_{\text{\tt mapid}}
  \end{array}\right]\rangle],
    [\langle [], [], a_{\text{\tt main}}\rangle],
    \left[\begin{array}{l}
      a_{\text{\tt id}} \mapsto \cdots \\
      a_{\text{\tt map1}} \mapsto \cdots \\
      a_{\text{\tt nil}} \mapsto \cdots \\
      a_{\text{\tt mapid}} \mapsto \cdots
    \end{array}\right],
    \sigma
  ) \\
  \Rightarrow
  \langle \mathbf{Eval}\,(\text{\tt let l = }\cdots)\,\left[\begin{array}{l}
    \text{\tt nil} \mapsto \mathbf{Addr}\,a_{\text{\tt nil}} \\
    \text{\tt mapid} \mapsto \mathbf{Addr}\,a_{\text{\tt mapid}} \\
    \text{\tt v} \mapsto \mathbf{Int}\,1
  \end{array}\right]\rangle(
    [],
    [],
    [\langle [], [], a_{\text{\tt main}}\rangle],
    \left[\begin{array}{l}
      a_{\text{\tt id}} \mapsto \cdots \\
      a_{\text{\tt map1}} \mapsto \cdots \\
      a_{\text{\tt nil}} \mapsto \cdots \\
      a_{\text{\tt mapid}} \mapsto \cdots
    \end{array}\right],
    \sigma
  ) \\
  \Rightarrow
  \langle \mathbf{Eval}\,(\text{\tt mapid \{l\}})\,\left[\begin{array}{l}
    \text{\tt nil} \mapsto \mathbf{Addr}\,a_{\text{\tt nil}} \\
    \text{\tt mapid} \mapsto \mathbf{Addr}\,a_{\text{\tt mapid}} \\
    \text{\tt v} \mapsto \mathbf{Int}\,1 \\
    \text{\tt l} \mapsto \mathbf{Addr}\,a_{\text{\tt l}}
  \end{array}\right]\rangle(
    [],
    [],
    [\langle [], [], a_{\text{\tt main}}\rangle],
    \left[\begin{array}{l}
      a_{\text{\tt id}} \mapsto \cdots \\
      a_{\text{\tt map1}} \mapsto \cdots \\
      a_{\text{\tt nil}} \mapsto \cdots \\
      a_{\text{\tt mapid}} \mapsto \cdots \\
      a_{\text{\tt l}} \mapsto \cdots
    \end{array}\right],
    \sigma
  ) \\
  \Rightarrow
  \langle \mathbf{Enter}\,a_{\text{\tt mapid}}\rangle(
    [\mathbf{Addr}\,a_{\text{\tt l}}],
    [],
    [\langle [], [], a_{\text{\tt main}}\rangle],
    \left[\begin{array}{l}
      a_{\text{\tt id}} \mapsto \cdots \\
      a_{\text{\tt map1}} \mapsto \cdots \\
      a_{\text{\tt nil}} \mapsto \cdots \\
      a_{\text{\tt mapid}} \mapsto \cdots \\
      a_{\text{\tt l}} \mapsto \cdots
    \end{array}\right],
    \sigma
  ) \\
  \Rightarrow
  \langle \mathbf{Eval}\,(\text{\tt map1 \{id\}})\,[]\rangle(
    [],
    [],
    [
      \langle [\mathbf{Addr}\,a_{\text{\tt l}}], [], a_{\text{\tt mapid}}\rangle,
      \langle [], [], a_{\text{\tt main}}\rangle
    ],
    \left[\begin{array}{l}
      a_{\text{\tt id}} \mapsto \cdots \\
      a_{\text{\tt map1}} \mapsto \cdots \\
      a_{\text{\tt nil}} \mapsto \cdots \\
      a_{\text{\tt l}} \mapsto \cdots
    \end{array}\right],
    \sigma
  ) \\
  \Rightarrow
  \langle \mathbf{Enter}\,a_{\text{\tt map1}}\rangle(
    [\mathbf{Addr}\,a_{\text{\tt id}}],
    [],
    [
      \langle [\mathbf{Addr}\,a_{\text{\tt l}}], [], a_{\text{\tt mapid}}\rangle,
      \langle [], [], a_{\text{\tt main}}\rangle
    ],
    \left[\begin{array}{l}
      a_{\text{\tt id}} \mapsto \cdots \\
      a_{\text{\tt map1}} \mapsto \cdots \\
      a_{\text{\tt nil}} \mapsto \cdots \\
      a_{\text{\tt l}} \mapsto \cdots
    \end{array}\right],
    \sigma
  ) \\
  \Rightarrow
  \langle \mathbf{Enter}\,a_{\text{\tt map1}}\rangle(
    [\mathbf{Addr}\,a_{\text{\tt id}}, \mathbf{Addr}\,a_{\text{\tt l}}],
    [],
    [\langle [], [], a_{\text{\tt main}}\rangle],
    \left[\begin{array}{l}
      a_{\text{\tt id}} \mapsto \cdots \\
      a_{\text{\tt map1}} \mapsto \cdots \\
      a_{\text{\tt nil}} \mapsto \cdots \\
      a_{\text{\tt l}} \mapsto \cdots \\
      a_{\text{\tt mapid}} \mapsto \langle \text{\tt \{f\} \textbackslash n \{xs\} -> } \cdots, [\mathbf{Addr}\,a_{\text{\tt id}}]\rangle
    \end{array}\right],
    \sigma
  ) \\
  \Rightarrow
  \langle \mathbf{Eval}\,(\text{\tt letrec mf = }\cdots)\,\left[\begin{array}{l}
    \text{\tt f} \mapsto \mathbf{Addr}\,a_{\text{\tt id}} \\
    \text{\tt xs} \mapsto \mathbf{Addr}\,a_{\text{\tt l}}
  \end{array}\right]\rangle(
    [],
    [],
    [\langle [], [], a_{\text{\tt main}}\rangle],
    \left[\begin{array}{l}
      a_{\text{\tt id}} \mapsto \cdots \\
      a_{\text{\tt map1}} \mapsto \cdots \\
      a_{\text{\tt nil}} \mapsto \cdots \\
      a_{\text{\tt l}} \mapsto \cdots \\
      a_{\text{\tt mapid}} \mapsto \cdots
    \end{array}\right],
    \sigma
  ) \\
  \Rightarrow
  \langle \mathbf{Eval}\,(\text{\tt mf \{xs\}})\,\left[\begin{array}{l}
    \text{\tt f} \mapsto \mathbf{Addr}\,a_{\text{\tt id}} \\
    \text{\tt xs} \mapsto \mathbf{Addr}\,a_{\text{\tt l}} \\
    \text{\tt mf} \mapsto \mathbf{Addr}\,a_{\text{\tt mf}}
  \end{array}\right]\rangle(
    [],
    [],
    [\langle [], [], a_{\text{\tt main}}\rangle],
    \left[\begin{array}{l}
      a_{\text{\tt id}} \mapsto \cdots \\
      a_{\text{\tt map1}} \mapsto \cdots \\
      a_{\text{\tt nil}} \mapsto \cdots \\
      a_{\text{\tt l}} \mapsto \cdots \\
      a_{\text{\tt mapid}} \mapsto \cdots \\
      a_{\text{\tt mf}} \mapsto \cdots
    \end{array}\right],
    \sigma
  ) \\
  \Rightarrow
  \langle \mathbf{Enter}\,a_{\text{\tt mf}}\rangle(
    [\mathbf{Addr}\,a_{\text{\tt l}}],
    [],
    [\langle [], [], a_{\text{\tt main}}\rangle],
    \left[\begin{array}{l}
      a_{\text{\tt id}} \mapsto \cdots \\
      a_{\text{\tt map1}} \mapsto \cdots \\
      a_{\text{\tt nil}} \mapsto \cdots \\
      a_{\text{\tt l}} \mapsto \cdots \\
      a_{\text{\tt mapid}} \mapsto \cdots \\
      a_{\text{\tt mf}} \mapsto \cdots
    \end{array}\right],
    \sigma
  ) \\
  \Rightarrow
  \langle \mathbf{Eval}\,(\text{\tt case ys \{\} of} \cdots)\,\left[\begin{array}{l}
    \text{\tt f} \mapsto \mathbf{Addr}\,a_{\text{\tt id}} \\
    \text{\tt mf} \mapsto \mathbf{Addr}\,a_{\text{\tt mf}} \\
    \text{\tt ys} \mapsto \mathbf{Addr}\,a_{\text{\tt l}}
  \end{array}\right]\rangle(
    [],
    [],
    [\langle [], [], a_{\text{\tt main}}\rangle],
    \left[\begin{array}{l}
      a_{\text{\tt id}} \mapsto \cdots \\
      a_{\text{\tt map1}} \mapsto \cdots \\
      a_{\text{\tt nil}} \mapsto \cdots \\
      a_{\text{\tt l}} \mapsto \cdots \\
      a_{\text{\tt mapid}} \mapsto \cdots \\
      a_{\text{\tt mf}} \mapsto \cdots
    \end{array}\right],
    \sigma
  ) \\
  \Rightarrow
  \langle \mathbf{Eval}\,\text{\tt ys \{\}}\,\left[\begin{array}{l}
    \text{\tt f} \mapsto \mathbf{Addr}\,a_{\text{\tt id}} \\
    \text{\tt mf} \mapsto \mathbf{Addr}\,a_{\text{\tt mf}} \\
    \text{\tt ys} \mapsto \mathbf{Addr}\,a_{\text{\tt l}}
  \end{array}\right]\rangle(
    [],
    [\left\langle \begin{array}{l}
      \text{\tt Nil \{\} -> } \cdots \\
      \text{\tt Cons \{z, zs\} -> } \cdots
    \end{array}, \left[\begin{array}{l}
    \text{\tt f} \mapsto \mathbf{Addr}\,a_{\text{\tt id}} \\
    \text{\tt mf} \mapsto \mathbf{Addr}\,a_{\text{\tt mf}} \\
    \text{\tt ys} \mapsto \mathbf{Addr}\,a_{\text{\tt l}}
  \end{array}\right]\right\rangle],
    [\langle [], [], a_{\text{\tt main}}\rangle],
    \left[\begin{array}{l}
      a_{\text{\tt id}} \mapsto \cdots \\
      a_{\text{\tt map1}} \mapsto \cdots \\
      a_{\text{\tt nil}} \mapsto \cdots \\
      a_{\text{\tt l}} \mapsto \cdots \\
      a_{\text{\tt mapid}} \mapsto \cdots \\
      a_{\text{\tt mf}} \mapsto \cdots
    \end{array}\right],
    \sigma
  ) \\
  \Rightarrow
  \langle \mathbf{Enter}\,a_{\text{\tt l}}\rangle(
    [],
    [\left\langle \begin{array}{l}
      \text{\tt Nil \{\} -> } \cdots \\
      \text{\tt Cons \{z, zs\} -> } \cdots
    \end{array}, \left[\begin{array}{l}
    \text{\tt f} \mapsto \mathbf{Addr}\,a_{\text{\tt id}} \\
    \text{\tt mf} \mapsto \mathbf{Addr}\,a_{\text{\tt mf}} \\
    \text{\tt ys} \mapsto \mathbf{Addr}\,a_{\text{\tt l}}
  \end{array}\right]\right\rangle],
    [\langle [], [], a_{\text{\tt main}}\rangle],
    \left[\begin{array}{l}
      a_{\text{\tt id}} \mapsto \cdots \\
      a_{\text{\tt map1}} \mapsto \cdots \\
      a_{\text{\tt nil}} \mapsto \cdots \\
      a_{\text{\tt l}} \mapsto \cdots \\
      a_{\text{\tt mapid}} \mapsto \cdots \\
      a_{\text{\tt mf}} \mapsto \cdots
    \end{array}\right],
    \sigma
  ) \\
  \Rightarrow
  \langle \mathbf{Eval}\,(\text{\tt Cons \{v, nil\}})\,\left[\begin{array}{l}
    \text{\tt v} \mapsto \mathbf{Int}\,1 \\
    \text{\tt nil} \mapsto \mathbf{Addr}\,a_{\text{\tt nil}}
  \end{array}\right]\rangle(
    [],
    [\left\langle \begin{array}{l}
      \text{\tt Nil \{\} -> } \cdots \\
      \text{\tt Cons \{z, zs\} -> } \cdots
    \end{array}, \left[\begin{array}{l}
    \text{\tt f} \mapsto \mathbf{Addr}\,a_{\text{\tt id}} \\
    \text{\tt mf} \mapsto \mathbf{Addr}\,a_{\text{\tt mf}} \\
    \text{\tt ys} \mapsto \mathbf{Addr}\,a_{\text{\tt l}}
  \end{array}\right]\right\rangle],
    [\langle [], [], a_{\text{\tt main}}\rangle],
    \left[\begin{array}{l}
      a_{\text{\tt id}} \mapsto \cdots \\
      a_{\text{\tt map1}} \mapsto \cdots \\
      a_{\text{\tt nil}} \mapsto \cdots \\
      a_{\text{\tt l}} \mapsto \cdots \\
      a_{\text{\tt mapid}} \mapsto \cdots \\
      a_{\text{\tt mf}} \mapsto \cdots
    \end{array}\right],
    \sigma
  ) \\
  \Rightarrow
  \langle \mathbf{ReturnCon}\,\text{\tt Cons}\,[\mathbf{Int}\,1, \mathbf{Addr}\,a_{\text{\tt nil}}]\rangle(
    [],
    [\left\langle \begin{array}{l}
      \text{\tt Nil \{\} -> } \cdots \\
      \text{\tt Cons \{z, zs\} -> } \cdots
    \end{array}, \left[\begin{array}{l}
    \text{\tt f} \mapsto \mathbf{Addr}\,a_{\text{\tt id}} \\
    \text{\tt mf} \mapsto \mathbf{Addr}\,a_{\text{\tt mf}} \\
    \text{\tt ys} \mapsto \mathbf{Addr}\,a_{\text{\tt l}}
  \end{array}\right]\right\rangle],
    [\langle [], [], a_{\text{\tt main}}\rangle],
    \left[\begin{array}{l}
      a_{\text{\tt id}} \mapsto \cdots \\
      a_{\text{\tt map1}} \mapsto \cdots \\
      a_{\text{\tt nil}} \mapsto \cdots \\
      a_{\text{\tt l}} \mapsto \cdots \\
      a_{\text{\tt mapid}} \mapsto \cdots \\
      a_{\text{\tt mf}} \mapsto \cdots
    \end{array}\right],
    \sigma
  ) \\
  \Rightarrow
  \langle \mathbf{Eval}\,(\text{\tt let fz =} \cdots)\,\left[\begin{array}{l}
    \text{\tt f} \mapsto \mathbf{Addr}\,a_{\text{\tt id}} \\
    \text{\tt mf} \mapsto \mathbf{Addr}\,a_{\text{\tt mf}} \\
    \text{\tt ys} \mapsto \mathbf{Addr}\,a_{\text{\tt l}} \\
    \text{\tt z} \mapsto \mathbf{Int}\,1 \\
    \text{\tt zs} \mapsto \mathbf{Addr}\,a_{\text{\tt nil}}
  \end{array}\right]\rangle(
    [],
    [],
    [\langle [], [], a_{\text{\tt main}}\rangle],
    \left[\begin{array}{l}
      a_{\text{\tt id}} \mapsto \cdots \\
      a_{\text{\tt map1}} \mapsto \cdots \\
      a_{\text{\tt nil}} \mapsto \cdots \\
      a_{\text{\tt l}} \mapsto \cdots \\
      a_{\text{\tt mapid}} \mapsto \cdots \\
      a_{\text{\tt mf}} \mapsto \cdots
    \end{array}\right],
    \sigma
  ) \\
  \Rightarrow
  \langle \mathbf{Eval}\,(\text{\tt Cons \{fz, mfzs\}})\,\left[\begin{array}{l}
    \text{\tt f} \mapsto \mathbf{Addr}\,a_{\text{\tt id}} \\
    \text{\tt mf} \mapsto \mathbf{Addr}\,a_{\text{\tt mf}} \\
    \text{\tt ys} \mapsto \mathbf{Addr}\,a_{\text{\tt l}} \\
    \text{\tt z} \mapsto \mathbf{Int}\,1 \\
    \text{\tt zs} \mapsto \mathbf{Addr}\,a_{\text{\tt nil}} \\
    \text{\tt fz} \mapsto \mathbf{Addr}\,a_{\text{\tt fz}} \\
    \text{\tt mfzs} \mapsto \mathbf{Addr}\,a_{\text{\tt mfzs}}
  \end{array}\right]\rangle(
    [],
    [],
    [\langle [], [], a_{\text{\tt main}}\rangle],
    \left[\begin{array}{l}
      a_{\text{\tt id}} \mapsto \cdots \\
      a_{\text{\tt map1}} \mapsto \cdots \\
      a_{\text{\tt nil}} \mapsto \cdots \\
      a_{\text{\tt l}} \mapsto \cdots \\
      a_{\text{\tt mapid}} \mapsto \cdots \\
      a_{\text{\tt mf}} \mapsto \cdots \\
      a_{\text{\tt fz}} \mapsto \cdots \\
      a_{\text{\tt mfzs}} \mapsto \cdots
    \end{array}\right],
    \sigma
  ) \\
  \Rightarrow
  \langle \mathbf{ReturnCon}\,\text{\tt Cons}\,[\mathbf{Addr}\,a_{\text{\tt fz}}, \mathbf{Addr}\,a_{\text{\tt mfzs}}]\rangle(
    [],
    [],
    [\langle [], [], a_{\text{\tt main}}\rangle],
    \left[\begin{array}{l}
      a_{\text{\tt id}} \mapsto \cdots \\
      a_{\text{\tt map1}} \mapsto \cdots \\
      a_{\text{\tt nil}} \mapsto \cdots \\
      a_{\text{\tt l}} \mapsto \cdots \\
      a_{\text{\tt mapid}} \mapsto \cdots \\
      a_{\text{\tt mf}} \mapsto \cdots \\
      a_{\text{\tt fz}} \mapsto \cdots \\
      a_{\text{\tt mfzs}} \mapsto \cdots
    \end{array}\right],
    \sigma
  ) \\
  \Rightarrow
  \langle \mathbf{ReturnCon}\,\text{\tt Cons}\,[\mathbf{Addr}\,a_{\text{\tt fz}}, \mathbf{Addr}\,a_{\text{\tt mfzs}}]\rangle(
    [],
    [],
    [],
    \left[\begin{array}{l}
      a_{\text{\tt id}} \mapsto \cdots \\
      a_{\text{\tt map1}} \mapsto \cdots \\
      a_{\text{\tt nil}} \mapsto \cdots \\
      a_{\text{\tt l}} \mapsto \cdots \\
      a_{\text{\tt mapid}} \mapsto \cdots \\
      a_{\text{\tt mf}} \mapsto \cdots \\
      a_{\text{\tt fz}} \mapsto \cdots \\
      a_{\text{\tt mfzs}} \mapsto \cdots \\
      a_{\text{\tt main}} \mapsto \langle \text{\tt \{fz, mfzs\} \textbackslash n \{\} -> Cons \{fz, mfzs\}} , [\mathbf{Addr}\,a_{\text{\tt fz}}, \mathbf{Addr}\,a_{\text{\tt mfzs}}]\rangle
    \end{array}\right],
    \sigma
  )
  \end{array}

ヒープはサンクを潰す時ぐらいしか整理してないので，参照がなくなったものは随時消す GC を実装すれば簡単に実行マシンは作れそう．

まとめ
------

まとめたかったのは主に， Part III の具体的な実装の話だったんだが力尽きた．てことで今日はこれで．続きは書くかもしれないし，書かないかもしれない．

機会があればもうちょっと正確を期して，どっかに上げるかもしれない．

.. [#spineless-is-still] spineless は僕の認識が正しければ生きてるはずだが，G-machine の実装では spineless は普通だったみたいな話があり， spineless もそこまで気にする用語ではないって事ですね．
.. [#shared-term-graph] なお，元々 shared term graph という二重の意味が込められてたっぽい: https://gitlab.haskell.org/ghc/ghc/blob/ghc-8.6.4-release/compiler/stgSyn/StgSyn.hs#L4 ．こっちの言い方は今でも通用しそうだが， STG machine はこっちの言い方だと shared term graph machine という感じになりそうで， reduction が抜けてるのはなんかあれですね．
.. [#haskell-optimizing-community] Haskell 界には， Haskell 最適化委員会なる組織があるらしく，その人たちは Haskell プログラムでもそういうのを見通す力を持ってるらしい．
.. [#all-variables-were-heap-allocated] 厳密には全てがヒープ割り当てされてるとは限らなくて， case での束縛はスタックに積まれるみたいなこともある．
.. [#example-by-paper] 例は論文中から持ってきたもの．
