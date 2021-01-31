Happy のコンフリクトレポートと向き合う
======================================

:tags: Haskell, パーサ, Happy, LALR
:category: フレームワーク

Haskell には，Happy と言う名前のパーサジェネレータがある．基本的には，yacc と同じような使い勝手のジェネレータで，GHC で使用されているため Haskell のパーサジェネレータとしてはそこそこ実績がある．

Happy は意味アクション付き BNF から LALR(1) パーサを生成する．このパーサは基本的に，構文解析表を見ながら shift / reduce と呼ばれる操作を行っていく，shift-reduce パーサの一種になる．しかし，shift-reduce パーサの生成は，一般に BNF から一意に決まらないことがある．そのような BNF が与えられた場合，一般にジェネレータは警告をユーザに伝えつつ，標準的なパーサを出力するようになっていることが多く，Happy もそのようになっている．ただ，その標準的パーサが意図していない動きをする場合も多い．

今回は，Happy が出す警告の見方とその自分なりの解決方法を紹介する．以下の文献を主に参考にしている．

* https://www.haskell.org/happy/doc/html/
* `GHC の Parser.y <https://gitlab.haskell.org/ghc/ghc/-/blob/eb90d23911ee10868dc2c7cc27a8397f0ae9b41d/compiler/GHC/Parser.y>`_

なお、Happy 1.20 を前提に話す。

shift-reduce パーサ
-------------------

まずは shift-reduce パーサの簡単な動きを押さえておく．shift-reduce パーサと言っても，その解析方法や実装によって結構生成方法が違うが，今回は LALR(1) に焦点を当てる．今回はそこまで細かい話には立ち入らないので，大まかなイメージをつかんでもらえれば良い．

shift-reduce パーサは，その名の通り，

shift
    入力から1つ文字を持ってきて，葉を作ってスタックに入れる

reduce
    スタックから幾つか木を持ってきて，それを組み合わせた木を作りスタックに入れる

の2つの操作を繰り返しながら，パースを行うボトムアップ構文解析法を指す．動きのイメージは https://en.wikipedia.org/wiki/Shift-reduce_parser に割とわかりやすい表があるので，ちょっと拝借する．

::

    R1: assign ← ID '=' sums
    R2: sums ← sums '+' prods
    R3: sums ← prods
    R4: prods ← prods '*' value
    R5: prods ← value
    R6: value ← INT
    R7: value ← ID

という文法があるとする．大文字や ``'..'`` が終端記号，小文字で書かれたものが非終端記号だ．この文法から生成されるパーサは，だいたい以下のような動きをする:

=============================== =========== =================== =================
パーススタック                      先読み      残りの入力          操作
=============================== =========== =================== =================
                                ``ID``      ``= ID + ID * INT`` shift
``ID``                          ``=``       ``ID + ID * INT``   shift
``ID =``                        ``ID``      ``+ ID * INT``      shift
``ID = ID``                     ``+``       ``ID * INT``        reduce by (R7)
``ID = value``                  ``+``       ``ID * INT``        reduce by (R5)
``ID = prods``                  ``+``       ``ID * INT``        reduce by (R3)
``ID = sums``                   ``+``       ``ID * INT``        shift
``ID = sums +``                 ``ID``      ``* INT``           shift
``ID = sums + ID``              ``*``       ``INT``             reduce by (R7)
``ID = sums + value``           ``*``       ``INT``             reduce by (R5)
``ID = sums + prods``           ``*``       ``INT``             shift
``ID = sums + prods *``         ``INT``                         shift
``ID = sums + prods * INT``                                     reduce by (R7)
``ID = sums + prods * value``                                   reduce by (R5)
``ID = sums + prods``                                           reduce by (R2)
``ID = sums``                                                   reduce by (R1)
``assign``                                                      accept
=============================== =========== =================== =================

パーサは状態スタックを持っており、次にどういう操作を行うかは、先読み一文字とパーススタックの先頭要素、現在の状態から決まる。

Happy でパーサを記述する
------------------------

実際に Happy で shift-reduce パーサを生成し、どういう状態と操作が行われるかを見てみる。

.. code-block:: haskell

    -- Parser.y
    {
    module Parser where
    }

    %token
        ID  { TokId _ }
        INT { TokInt _ }
        '=' { TokSym "=" }
        '+' { TokSym "+" }
        '*' { TokSym "*" }

    %tokentype { Token }

    %name assign
    %%

    assign :: { (Token, Expr) }
        : ID '=' sums   { ($1, $3) }

    sums :: { Expr }
        : sums '+' prods    { Sum $1 $3 }
        | prods             { $1 }

    prods :: { Expr }
        : prods '*' value   { Prod $1 $3 }
        | value             { $1 }

    value :: { Expr }
        : ID                { Val $1 }
        | INT               { Val $1 }

    {
    happyError :: [Token] -> a
    happyError _ = error "happyError"

    data Token
        = TokId String
        | TokInt Integer
        | TokSym String
        deriving (Eq, Show)

    data Expr
        = Sum Expr Expr
        | Prod Expr Expr
        | Val Token
        deriving (Eq, Show)
    }

このプログラムに対して、

::

    happy -i Parser.y

を実行すると、パーサプログラム ``Parser.hs`` とパーサ生成時の情報 ``Parser.info`` が生成される。``Parser.info`` には文法解析時の情報と、状態遷移に関するものが書かれる。状態遷移に関するものは ``States`` 節に書かれている。今回は、

::

    -----------------------------------------------------------------------------
    States
    -----------------------------------------------------------------------------
    State 0


            ID             shift, and enter state 2

            assign         goto state 3

    State 1


            ID             shift, and enter state 2


    State 2

            assign -> ID . '=' sums                             (rule 1)

            '='            shift, and enter state 4


    State 3

            %start_parseAssign -> assign .                      (rule 0)

            %eof           accept


    State 4

            assign -> ID '=' . sums                             (rule 1)

            ID             shift, and enter state 8
            INT            shift, and enter state 9

            sums           goto state 5
            prods          goto state 6
            value          goto state 7

    State 5

            assign -> ID '=' sums .                             (rule 1)
            sums -> sums . '+' prods                            (rule 2)

            '+'            shift, and enter state 11
            %eof           reduce using rule 1


    State 6

            sums -> prods .                                     (rule 3)
            prods -> prods . '*' value                          (rule 4)

            '+'            reduce using rule 3
            '*'            shift, and enter state 10
            %eof           reduce using rule 3

    ...

みたいな情報が出力される．それぞれの状態について，

* State の値
* reduce 途中の規則
* 先読み値での操作
* パーススタックの状態による操作

が表示されている．reduce 途中の規則表示は，``.`` の位置までは reduce 済みということを表している．この辺は LALR 法に馴染みのある人なら分かると思うが，状態の作り方に由来する情報で，解析するプログラムが大きくなるとあまり役に立つ情報ではなかったりするが，覚えておいて損はないだろう．

コンフリクトとその解決
----------------------

さて，ここからが本題．shift-reduce パーサ生成では，パーサが上記の通りの動きをすることから，先読みしている文字から shift か reduce のどちらを行うか一意に決めなければならない．しかし，書かれた文法によっては，うまく一意に決められないことがある．大方問題になるのは以下のケースだ:

* 文法が曖昧性を含んでいる
* 先をパースして見なければ，操作が決定できない

一般的に，shift-reduce パーサ生成器はこのような問題を直接検出はしてくれない場合が多い．特に，Happy の場合は，直接検出の機能はない．ただ代わりに，これらの問題は間接的に shift/reduce 衝突，reduce/reduce 衝突という2つの警告によって報告される．

それぞれの警告は，名前の通り

shift/reduce 衝突
    shift 操作と reduce 操作，どちらも可能．この場合，自動的に shift 操作が選択される．

reduce/reduce 衝突
    可能な reduce 操作が複数ある．この場合，自動的に一番早く現れる規則に関する reduce 操作が選択される．

という問題を表している．実際に，衝突が報告されるようなプログラムを書いてみる:

.. code-block:: haskell

    -- Parser.y
    {
    module Parser where
    }

    %token
        ID  { TokId _ }
        '+' { TokSym "+" }

    %tokentype { Token }

    %name expr
    %%

    expr :: { Expr }
        : expr '+' expr     { Sum $1 $3 }
        | ID                { Val $1 }

    {
    happyError :: [Token] -> a
    happyError _ = error "happyError"

    data Token
        = TokId String
        | TokSym String
        deriving (Eq, Show)

    data Expr
        = Sum Expr Expr
        | Val Token
        deriving (Eq, Show)
    }

これの ``Parser.info`` を見ると，最初に

::

    state 6 contains 1 shift/reduce conflicts.

というように shift/reduce 衝突が報告される．State 6 の情報を見に行ってみると，

::

    State 6

	expr -> expr . '+' expr                             (rule 1)
	expr -> expr '+' expr .                             (rule 1)

	'+'            shift, and enter state 5
			(reduce using rule 1)

	%eof           reduce using rule 1

という表示がある．``'+'`` の操作で ``(reduce using rule 1)`` と言う表記があるのが分かるだろう．衝突している操作で，選ばれなかったものは ``(...)`` と言う形で表記される．今回の場合は，

* ``shift, and enter state 5``
* ``reduce using rule 1``

が衝突し，shift/reduce 衝突なので shift が優先されている．このケースは文法自体に曖昧性がある．つまり，``a + b + c`` のような式が，``Sum (Sum a b) c`` とパースされて欲しいのか，``Sum a (Sum b c)`` とパースされて欲しいのか分からないのだ．``'+'`` が出てきた時に shift を優先させると，shift を貪欲的に行い最終的にまとめて reduce されるため ``Sum a (Sum b c)`` が結果になる．実際，今回のパース結果を見てみると，

::

    >>> expr [TokId "a", TokSym "+", TokId "b", TokSym "+", TokId "c"]
    Sum (Val (TokId "a")) (Sum (Val (TokId "b")) (Val (TokId "c")))

となっている．reduce が優先された場合，まず出来上がったところから reduce していくため，``Sum (Sum a b) c`` が結果になる．もし，shift 優先で問題ないなら，それを Happy に ``%shift`` ディレクティブで以下のように教えることができる:

.. code-block:: haskell

    expr :: { Expr }
        : expr '+' expr %shift  { Sum $1 $3 }
        | ID                    { Val $1 }

または，文法から曖昧性を排除することで，解決できる．こちらの方が一般的な解決方法だろう．左結合でのパース結果，つまり reduce 優先した場合の結果が得たかったら次のように書けば良い:

.. code-block:: haskell

    expr :: { Expr }
        : expr '+' ID   { Sum $1 (Val $3) }
        | ID            { Val $1 }

shift/reduce 衝突は基本避けられるなら避けた方がいいが，そこまで無理をして避ける必要はないだろう．shift/reduce を無理に避けるため文法が複雑になるより，``%shift`` ディレクティブを指定した方がプログラムが保守しやすい場合も多い．特に，演算子・ブロックの優先順位の問題においては，``%shift`` ディレクティブや ``%left``，``%right`` を使用した方が意味アクションが書きやすい場合が多い．

しかしながら，reduce/reduce 衝突の方は，プログラマが意図しないパーサが生成されるケースが多いため，注意が必要だ．例えば問題になるのは，以下のような文法だ:

.. code-block:: haskell

    expr :: { Expr }
        : value '+' expr            { Sum $1 $3 }
        | value '+' value           { Sum $1 $3 }
        | value                     { $1 }

    value :: { Expr }
        : ID                        { Val $1 }

この文法では，

::

    state 10 contains 1 reduce/reduce conflicts.

という警告が生成される．State 10 は次のような情報表示がされる::

    State 10

        expr -> value . '+' value                           (rule 1)
        expr -> value '+' value .                           (rule 1)
        expr -> value . '+' expr                            (rule 2)
        expr -> value .                                     (rule 3)

        '+'            shift, and enter state 6
        %eof           reduce using rule 3
                (reduce using rule 1)

この場合，``a + b`` という式を，

* ``expr -> value '+' expr`` の規則で reduce するか
* ``expr -> value '+' value`` の規則で reduce するか

が分からない点で，文法が曖昧になっている．ただ，警告自体は，

* ``expr -> value '+' value``
* ``expr -> value``

の2つが衝突していると言っている事には注意が必要だ．実際には，後者のルールは reduce が起きた後 ``expr -> value '+' expr`` で reduce が走ることを前提にしているわけだが，警告をそのまま素直に受け取ると，問題の特定が難しい場合があるのには注意だ．さて，今回のケースでは2つの reduce が被らないように文法を修正するのが良いだろう．具体的には，

.. code-block:: haskell

    expr :: { Expr }
        : value '+' value '+' expr2     { Sum $1 $3 }
        | value '+' value               { Sum $1 $3 }
        | value                         { $1 }

    expr2 :: { Expr }
        : value '+' expr2               { Sum $1 $3 }
        | value                         { $1 }

    value :: { Expr }
        : ID                            { Val $1 }

というように修正するのが良いだろう．

基本的に，自身が管理できていない衝突は，その原因を特定して解決しておいた方がいい．特に，reduce/reduce 衝突は解決しておく必要があるだろう．Happy はデフォルトでは衝突を検知してもパーサをそのまま生成するが，このようなマナーを考慮して，``%expect`` ディレクティブというものを用意してくれている．これを使って，

.. code-block:: haskell

    %expect 10

というように書くと，reduce/shift 衝突が 10 箇所，reduce/reduce 衝突がない場合のみパーサを生成するようになる．基本的に，reduce/shift 衝突は，その起こっている箇所に ``%shift`` ディレクティブを指定するのがいいだろう．なので，基本的に，

.. code-block:: haskell

    %expect 0

を指定し，``%shift`` ディレクティブを適宜入れながら，reduce/reduce 衝突は回避していくのが良いだろう．

まとめ
------

今回は shift-reduce 生成器 Happy で起こる shift/reduce 衝突，reduce/reduce 衝突の回避方法を軽く紹介した．基本的に，衝突は回避していくのが良いが，shift/reduce 衝突については無理に回避せず ``%shift`` を使うという手がある．また，``%expect 0`` はつけておいた方が良いだろう．

文法が大きくなってくると，State の情報で提示された reduce の対象ルールと実際問題がある曖昧性のあるルールは割とかけ離れていたりするので注意が必要だ．基本的には，先読み対象のトークンが使われている場所を探索し，地道に曖昧性がある部分を探していくしかないと思う．また，実際には先読みを十分に行えば判別可能な曖昧性についても，衝突を起こしてしまう場合がある．その場合は，効率的なパースのためにも早期に判別可能な文法に修正していくのが良いだろう．この辺は，PEG とかと比べると少々使い勝手悪いっすね．というわけで，今日はこの辺で．
