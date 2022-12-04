BlockArguments で括弧を書かない生活を送る
=========================================

:tags: Haskell, GHC, GHC拡張
:category: プログラミング言語

GHC 8.6 から ``BlockArguments`` という拡張が入った．これは構文拡張で，Haskell 2010 で必要だった幾つかの括弧を省略できるようにするものだ．この拡張を使うことで，Haskell プログラミングにおいて極限まで括弧を省略できるようになった．今回は，``BlockArguments`` の簡単な紹介と，どういう風に括弧の省略ができるかを見ていきたいと思う．

``BlockArguments`` 拡張
-----------------------

Haskell 2010 では，構文要素として幾つかのブロック式が入っている．

.. code-block:: text

  lexp  →  \ apat1 … apatn -> exp
        |  let decls in exp
        |  if exp [;] then exp [;] else exp
        |  case exp of { alts }
        |  do { stmts }
        |  fexp

における，

ラムダ式
  ``\ apat1 … apatn -> exp``

``let`` 式
  ``let decls in exp``

``if`` 式
  ``if exp [;] then exp [;] else exp``

``case-of`` 式
  ``case exp of { alts }``

``do`` 式
  ``do { stmts }``

の5種類の式だ．他に，GHC 拡張で ``mdo``，``\case``，``proc`` などもある．ところで，これらの式は ``lexp`` に紐づいている．これは，中置記法構文において

.. code-block:: text

  infixexp  →  lexp qop infixexp
            |  - infixexp
            |  lexp

のように，左の式の構文要素として使用される．具体的には，

.. code-block:: haskell

  atomically $ do
    v <- readTVar tv
    writeTVar tv $! v + 1

のように書ける．なお，

.. code-block:: haskell

  \f -> f $ 0

のような式の場合，``(\f -> f) $ 0`` か ``\f -> (f $ 0)`` の解釈の2通りが存在し，曖昧性が生じるが，これを解決するためのメタ規則が設定されており [#haskell2010-metarule]_，ブロック式は可能な限り，右に広くとることになっているため，この式は ``\f -> (f $ 0)`` に一意に解釈が決まる．よって，左にブロック式を書きたい場合は，

.. code-block:: haskell

  (\f -> f) $ 0

のように括弧で囲む必要がある．ところで，先ほどの

.. code-block:: haskell

  atomically $ do
    v <- readTVar tv
    writeTVar tv $! v + 1

の式は，

.. code-block:: haskell

  atomically do
    v <- readTVar tv
    writeTVar tv $! v + 1

のように，``$`` 無しで書くことはできない．関数適用の構文は，

.. code-block:: text

  fexp  →  [fexp] aexp

  aexp  →  qvar
        |  gcon
        |  literal
        |  ( exp )
        |  …

のようになっている．``aexp`` の中で ``lexp`` の構文を使用するには，括弧で囲む必要があるため，関数適用を ``$`` 無しで書くには，

.. code-block:: haskell

  atomically (do
    v <- readTVar tv
    writeTVar tv $! v + 1
  )

のように括弧で囲む必要がある．``BlockArguments`` の概要は，ブロック式を ``lexp`` から ``aexp`` に移すことで，関数適用時にも括弧無しで書けるようにするというものだ．結果，

.. code-block:: haskell

  atomically do
    v <- readTVar tv
    writeTVar tv $! v + 1

という式が，有効になる．これが，``BlockArguments`` 拡張になる．

``do`` によるレイアウトハッキング
---------------------------------

さて，``BlockArguments`` はここまで見ると単純な拡張に見える．ところが，これを ``do`` 構文と組み合わせると，今までの ``$`` による書き方を書きやすくするのに加え，それより遥かに豊かな表記法を提供してくれる．例えば，以下のような書き方が可能になる:

.. code-block:: haskell

  foldl'
    do \acc x -> acc . shows x
    do \x -> x
    do replicate 10 1
    do ""

この式は，

.. code-block:: haskell

  foldl'
    (\acc x -> acc . shows x)
    (\x -> x)
    (replicate 10 1)
    ""

と同等になる．そのカラクリは，``do`` 式の脱糖方法にある．``do`` 式は，構成要素が1つの式しか無い場合，何も行わずその式を展開する．``do`` 式の脱糖が意味を持つのは2つ以上式が並んだ場合のみだ．それを利用すれば，モナドと何の関係性もない式を，``do`` で使うことにより，ある式の範囲を明示することができる．

``$`` は演算子の優先順位により，両側の式範囲を示すマーカーとなるが，``do`` は代わりにレイアウトルールによりその範囲を明示するのに使用できる．ただ，この違いは注意が必要でもある．例えば，

.. code-block:: haskell

  f . g . h $ x

という式は，

.. code-block:: haskell

  (f . g . h) $ x

と同等の式になる．ところが，``$`` を何も考えずに

.. code-block:: haskell

  f . g . h do x

というように，``do`` に置き換えるだけでは，

.. code-block:: haskell

  f . g . (h x)

というように，元の式とは異なる式になってしまう．このような場合は，``$`` を置き換えることはできない．また，

.. code-block:: haskell

  id $ Identity
    { runIdentity = 0
    }

というような式を ``do`` で書く場合，

.. code-block:: haskell

  id do Identity
    { runIdentity = 0
    }

と書いてしまうと，パースエラーになる．``do`` 式は ``{`` を省略した場合はレイアウトルールにより解釈が行われる．上の式は，``runIdentity`` の前の ``{`` が ``do`` の現在の位置より前にあるため，その位置でレイアウトが閉じてしまい，

.. code-block:: haskell

  id do { Identity }
    { runIdentity = 0
    }

のように解釈されることになってしまう．よって，この場合は，

.. code-block:: haskell

  id do
    Identity
      { runIdentity = 0
      }

のように，一続きになる式をインデントを意識して書く必要がある．``runIdentity`` の前の ``{`` は，``do`` の位置より深くする，つまり ``Identity`` を書いた位置より深いところで書く必要がある．ただ，これは使いようによっては，

.. code-block:: haskell

  foldl'
    do \acc x ->
          acc . shows x
    do \x -> x
    do replicate 10 1
    do ""

のように複数行のブロックの範囲を明示することもできる．

これを使えば，例えば ``optparse-applicative`` などの applicative style を多用するライブラリで，以下のように括弧をなくすことができる:

.. code-block:: haskell

  sample = Sample
    <$> strOption do
        long "hello"
          <> metavar "TARGET"
          <> help "Target for the greeting"
    <*> switch do
        long "quiet"
          <> short 'q'
          <> help "Whether to be quiet"
    <*> option auto do
        long "enthusiasm"
          <> help "How enthusiastically to greet"
          <> showDefault
          <> value 1
          <> metavar "INT"

これは，演算子の優先順位の関係で ``$`` を使っての括弧の省略はできないため，通常は，

.. code-block:: haskell

  sample = Sample
    <$> strOption
        ( long "hello"
        <> metavar "TARGET"
        <> help "Target for the greeting"
        )
    <*> switch
        ( long "quiet"
        <> short 'q'
        <> help "Whether to be quiet"
        )
    <*> option auto
        ( long "enthusiasm"
        <> help "How enthusiastically to greet"
        <> showDefault
        <> value 1
        <> metavar "INT"
        )

のように括弧をつける必要があった．それを，``BlockArguments`` による ``do`` 構文を使用することで，``$`` より多彩な表現ができるようになる．また，前に紹介した

.. code-block:: haskell

  f . g . h $ x

も実は，

.. code-block:: haskell

  do f . g . h
  x

というように書き方を工夫すれば，括弧や ``$`` を省略できる．ただ，やりすぎは可読性を著しく落とすので，用量用法を守って使用すると良いと思う．

まとめ
------

今回は，``BlockArguments`` の紹介と，その拡張下での ``do`` 構文とレイアウトルールによる，括弧や ``$`` の省略法を紹介した．実は最近この方法を使っていて，完全に ``$`` を使わなくても大体書けるなあって感じになった．

基本的には，ある式の範囲を明示したい時は，

* ``do`` から始めて，複数行に渡る場合は最初のインデントより以降の行のインデントを深くする
* その式の範囲が終わったら，改行して今までのインデントより浅いところから始める

ということをすれば，括弧無しでレイアウトルールに任せた式表記ができる．用量用法を守って使っていきたい．こちらからは以上です．

.. [#haskell2010-metarule] https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-220003
