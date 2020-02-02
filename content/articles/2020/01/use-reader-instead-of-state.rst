State モナドの代わりに Reader モナドを使う
==========================================

:date: 2020-01-30 20:00
:tags: Haskell, GHC, モナド
:category: プログラミング

**注意**
  この記事の主張には不備が確認されています．不備のある箇所は，FIXME に付属してその不備の内容が示されています．この不備は近いうちに修正する予定です．

Haskell で State モナドはモナドの代表格だ．Haskell 入門者は，多くの場合，状態を伴った計算を State モナドで書くことを習うだろう．しかし，実用上の多くの場面では，State モナドではなく他の選択肢を選んだ方がいい場合がある．一つの選択肢が，Reader モナドと可変参照を使う方法だ．今回は，この手法を使う利点と利用場面について考えていこうと思う．

なお，環境として以下を想定している．

+--------------------+---------+
| GHC のバージョン   | 8.6.5   |
+--------------------+---------+
| Cabal のバージョン | 3.0.0.0 |
+--------------------+---------+

IORef の神話
------------

Haskell はかなり多くの神話を持つ言語だ．その中の一つに，以下のものがある．

  IORef を使うとパフォーマンスは良いが純粋性を損なう．なので，パフォーマンスを気にしないなら State モナドを使うべきだ．

今回の記事は，パフォーマンスについても IORef が State モナドより良い場合があるという主張を含むので，この神話に加担する側面がある．なので，初めに注意喚起を載せておく．巷の多くの例では，この言葉は都市伝説に過ぎない．まずは，その辺について見ておこう．これは，可変参照と State モナドの違いを理解する上でも役に立つはずだし，本記事の良い導入となるだろう．

さて，以下のサンプルコードを見てみる:

.. code-block:: haskell

  import Control.Monad.State.Strict
  import Data.IORef

  sumByState :: [Int] -> Int
  sumByState l = execState (go l) 0
    where
      go []     = pure ()
      go (x:xs) = do
        modify' \s -> s + x
        go xs

  sumByIORef :: [Int] -> IO Int
  sumByIORef l = do
      r <- newIORef 0
      go r l
      readIORef r
    where
      go _ []     = pure ()
      go r (x:xs) = do
        modifyIORef' r \s -> s + x
        go r xs

この場合，``sumByState`` と ``sumByIORef`` でどちらがどれくらい良いパフォーマンスを期待できるだろうか？ まずは，実測してみよう．`criterion <https://hackage.haskell.org/package/criterion>`_ を使って，ベンチマークコードを書いてみる:

.. code-block:: haskell

  module Main where

  import qualified Criterion.Main as Criterion

  main :: IO ()
  main = Criterion.defaultMain
      [ bgroup "sumByState" $ Criterion.nf sumByState
      , bgroup "sumByIORef" \xs -> Criterion.nfIO $ sumByIORef xs
      ]
    where
      bgroup n f = Criterion.bgroup n $ benches f

      benches f =
        [ Criterion.bench ("n=" ++ show n) $ f xs
        | (n, xs) <- samples
        ]

      samples = do
        i <- [1..5]
        let n = 10 ^ i
        pure (n, [1..n])

このコードによる測定結果を，グラフにプロットしてみると以下のようになる:

.. image:: {attach}use-reader-instead-of-state/state-vs-ioref.png
  :alt: ``sumByState`` より ``sumByIORef`` の時間の方が常に長い
  :align: center

どちらの軸も対数でとってることに注意．見ての通り，常に ``sumByState`` の方が ``sumByIORef`` より速い．この場合，State モナドは，IORef を使う場合に比べ，「パフォーマンスが良くて，しかも純粋性を損なわない」わけだ．さて，なぜこのような結果になるのだろう？ Haskell は純粋性を持つコードを贔屓しているのだろうか？ それとも State モナドの実装は，裏で高度な技術が使われていて，それによって高効率な動作をするようになっているのだろうか？

実は，State モナドと IORef のコードは，やってることはそう違いはない．これは，大局的に見てという話ではなく，本当にそれぞれの操作が割と対応したコードになっているのだ．にも関わらず違いが出るのは何故なのだろう？ このカラクリを紐解いていこう．GHC では通常最適化レベル 1 でコンパイルをするわけだが，まずは他のレベルでどうなるか見てみよう．``cabal`` を使っている場合は，

::

  cabal build --enable-optimization=n && cabal --enable-optimization=n exec ...

というようにすれば，試せる．結果は，大体以下のような結果になる:

最適化レベル2
  ``sumByIORef`` の方はあまり変わらないが，``sumByState`` はさらに性能が良くなり，両者の性能差が浮き彫りになる．

最適化レベル0
  ``sumByIORef`` の方が ``sumByState`` に比べ2倍ほど速くなる．

最適化レベル0の結果から分かる通り，実は State モナドと IORef の性能差は，最適化によって生まれる．最適化レベル1において， ``sumByState`` / ``sumByIORef`` はそれぞれ概ね次のようなコードに最適化される:

.. code-block:: haskell

  {-# LANGUAGE MagicHash     #-}
  {-# LANGUAGE UnboxedTuples #-}

  import GHC.Base
  import GHC.Prim

  sumByState :: [Int] -> Int
  sumByState l = case go l 0 of
      (# _, r #) -> r
    where
      go :: [Int] -> Int -> (# (), Int #)
      go xs acc = case xs of
        []   -> (# (), acc #)
        y:ys -> go ys (acc + y)

  sumByIORef :: [Int] -> IO Int
  sumByIORef l = IO
      \s0 -> case newMutVar# 0 s0 of
      { (# s1, r# #) -> case go (coerce (STRef r#)) l s1 of
      { (# s2, _  #) -> readMutVar# r s2
      }
      }
    where
      go :: IORef Int -> [Int] -> State# RealWorld -> (# State# RealWorld, () #)
      go r xs s0 = case xs of
        []   -> (# s0, () #)
        y:ys -> case coerce r of
          r'@(STRef r#) -> case readMutVar# r# s0 of
          { (# s1, acc #) -> case writeMutVar# r# (acc + y) of
          { s2 -> go (coerce r') ys s2
          }
          }

幾人かは，このコードがまだ最適化の余地を残していることに気づくだろう．実際，最適化レベル2では，さらに w/w という最適化が入り，どちらも余計な処理が省かれる．特に， ``sumByState`` はその省かれた処理によって，かなり高速化される．ところでパッと見で， ``sumByState`` より ``sumByIORef`` の最適化コードは複雑に見える．多くの場合，ミクロな視点で見れば，コードは複雑なものより単純なものの方が速い．実際今回は， ``sumByState`` が ``sumByIORef`` の方が優っていた．ところが，この2つのコード，実際にやっていることにそう違いはない．IORef は，通常あるヒープ領域を指すポインタとして実装される．そして，そのヒープ領域には，実データのクロージャを指すポインタがやっぱり入っている．そこで，両者の違いは，

* 実データのクロージャを指すポインタを直接参照するか，一旦実データを指すポインタを参照しさらにそこから実データを参照する2段階の参照か
* Haskell の単純な構文を保っているか，``readMutVar#`` / ``writeMutVar#`` などのプリミティブな命令が露出しているか

になる．ただ，2段階の参照になっても他の処理と比べて相対的にそこまで遅くなるというわけではなく，プリミティブな命令もコード生成時には単純にメモリ参照の命令に置き換わり，特別なランタイム API の呼び出しなどは通常起きない．なので，これらの違いは直接的にはパフォーマンスの違いに影響を及ばさないのだが，間接的には大きな影響を与える．一般に，GHC で IORef が通常の State モナドよりパフォーマンスで劣る点は以下のものだ:

* プリミティブな命令をコード生成まで展開できず，そこまでの最適化が阻害される．
* ``writeMutVar#`` は重い処理であり，単純に参照を取り換えるだけでなく，GC への特別な処理を要求する [#notice-write-barrier]_ ．

1つ目の問題は，最適化レベル1ではそこまで現れていないが，最適化レベル2で ``sumByIORef`` が ``sumByState`` に性能差を広げられたことの主な要因になる．w/w 変換で， ``sumByState`` は次のように最適化できる:

.. code-block:: haskell

  sumByState :: [Int] -> Int
  sumByState l = case go l 0# of
      (# _, r #) -> r
    where
      go :: [Int] -> Int# -> (# (), Int #)
      go xs acc# = case xs of
        []   -> (# (), I# acc# #)
        y:ys -> case y of
          I# y# -> go ys (acc# +# y#)

本来の ``+`` では， ``I#`` を取って ``+#`` で計算した結果をまた ``I#`` で包むといったことをしてたのが，こうするとその処理が省ける他， ``Int#`` の値をいちいちヒープに入れなくて良くなり，かなり速度の改善が見込める．実際，速度はかなり改善する．IORef の方も似たようなことをやってるので，このような最適化を適用しようと思えばできるのだが，残念ながら適用されない．なぜなら，プリミティブ命令は実行コード生成時まで展開されず，最適化の適用にはプリミティブ命令の展開が必要だが，最適化は実行コード生成前に行われるため，結果的にミスマッチにより最適化が適用されないということが起こるからだ．つまり，最適化レベル2での ``sumByState`` と ``sumByIORef`` の対決は，片方は ``I#`` によるラップ処理を省きスタックへの参照のみで完結，もう片方は ``I#`` のラップ処理が必要で一々ヒープに書き込む操作も必要といったプログラム同士の悲惨な対決となってしまう．

2つ目の問題は，知るところでは知られた問題だ．一般に writeIORef は幾つかの側面から多用はやめた方がいいと言う通説がある．これは，

.. code-block:: haskell

  sumByState :: Int -> Int
  sumByState n = evalState (go (10 * n) 0) 0
    where
      go 0 !y = pure y
      go m !y = do
        s <- get
        when (m `mod` n == 0)
          do put $! s + 1
        go (m - 1) (y + s)

  sumByIORef :: Int -> IO Int
  sumByIORef n = do
      r <- newIORef 0
      go r (10 * n) 0
    where
      go _ 0 !y = pure y
      go r m !y = do
        s <- readIORef r
        when (m `mod` n == 0)
          do writeIORef r $! s + 1
        go r (m - 1) (y + s)

のように， ``writeIORef`` を抑えたプログラムで速度を実測してみると分かるが，この場合当初は 40% ほどの性能差だったのが 10% ほどになる．つまり，最適化レベル1 での性能悪化の主な要因は，大雑把に言えば ``writeIORef`` の多用にあると言うことだ．

これまでの議論 [#notice-optimization-0]_ から，IORef が State モナドよりパフォーマンスの悪化を招きやすい，少なくとも State モナドより速くなることはないというのが，大方の結論になる．よって，GHC では，IORef より State モナドを使う方が，純粋な計算で完結でき，しかも速いのだ．これが，最初の話題が神話である所以だ．

IORef 再考
----------

という話で終わると，単なる注意喚起になってしまうのだが，本題はここからだ．さて，IORef の問題点は以下の2点だった．

* プリミティブな命令をコード生成まで展開できず，そこまでの最適化が阻害される．
* ``writeMutVar#`` は重い処理であり，あまり多用してはいけない．

これは，別の言い方をすれば，

* プリミティブ命令を展開するような最適化が，そこまでパフォーマンスに大きく影響しない
* ``writeMutVar#`` をそこまで多用しない

コードであれば，IORef は有効ということになるのではないだろうか？ 1つ目の用件は，スタック領域だけで完結しないような状態，つまり ``Int`` のようなものでなく ``Bool`` のような本質的に boxed なデータを扱うコードであれば，大体クリアできる． ``writeMutVar#`` についても，頻繁に変更しないが，参照は頻繁に行うような要件はいくらでもあるだろう．特に，今回対象にしたいのが，グローバルコンテキストだ．グローバルコンテキストの賛否はともかくとして，現実の多くのプログラムは，巨大で常駐し続けるプログラムの設定を管理するデータを持っている．通常グローバルコンテキストは，幾つかのフィールドから構成されていて，ネストされていたりもする．フィールドの中身はヒープに確保しなければいけないため，1つ目の条件を満たす．さらに，その中の幾つかのフィールドは変更可能なものになっている場合があり，起動してからいくつかのタイミングで更新される可能性がある．しかし，それほど頻繁な変更ではないため，2つ目の条件も満たすことになる．つまり，グローバルコンテキストは先ほど挙げた2点を満たしているのだ．さらに嬉しいことに，このような状況設定だと，むしろ State モナドより IORef の方がパフォーマンスを発揮する場合がある．先程の例で，状態を次のようなものにしてみる:

.. code-block:: haskell

  data Context a = Context
    { subctx :: SubContext a
    , param1 :: Bool
    , param2 :: String
    }

  data SubContext a = SubContext
    { subparam1 :: a
    , subparam2 :: Bool
    }

  initialContext x = Context
    { subctx = SubContext
        { subparam1 = x
        , subparam2 = False
        }
    , param1 = True
    , param2 = ""
    }

  sumByState :: [Int] -> Context Int
  sumByState l = execState (go l) (initialContext 0)
    where
      go []     = pure ()
      go (x:xs) = do
        modify' \ctx -> ctx
          { subctx = (subctx ctx)
              { subparam1 = subparam1 (subctx ctx) + x
              }
          }
        go xs

  sumByIORef :: [Int] -> IO (Context (IORef Int))
  sumByIORef l = do
      r <- newIORef 0
      let ctx = initialContext r
      go ctx l
      pure ctx
    where
      go _   []     = pure ()
      go ctx (x:xs) = do
        let r = subparam1 $ subctx ctx
        modifyIORef' r \s -> s + x
        go ctx xs

**FIXME**
  以降の考察は，パフォーマンス面においては `間違い <https://github.com/mizunashi-mana/blog/pull/85#discussion_r373787772>`_ が指摘されています．``sumByState`` は，速度の遅さを改善できる箇所があり，その場合 ``sumByIORef`` より速くなります．この部分については，現在調査中で，別の例になるか，または考察を修正する必要があります．

この場合，最適化レベル1 / 2 両方で， ``sumByIORef`` は ``sumByState`` の 10 倍以上速くなる．これは，State モナドの方の場合，更新のたびに一々 ``SubContext`` / ``Context`` を作り直さなければならないのに対し，IORef は対象の部分だけを更新すれば良く，他の部分は完全に共有されるからだ．これは，メモリ変更の局所性にも貢献する．つまり，最初に挙げた神話は，状態の取り扱い次第では事実になることもある．特に，現実のコードでは，真になる場合も多い．また，このようにネストされたデータの一部分が変更可能な場合，そこを IORef にすることは，パフォーマンスだけでなくコードの簡潔さにも貢献する．State モナドの場合，どこが変更可能なフィールドなのか，データ定義だけから判別はできないが，IORef にすることで，可変なフィールドを明示することができる．また，見ての通り，State モナドの例では読み込み，書き込み両方でデータのネスト構造を辿る必要があったのが，IORef では読み込みのみで良くなり，ノイズを減らすことにも貢献している．

Reader + IORef
--------------

さて，先ほど挙げた ``sumByIORef`` は Reader モナドを使うと次のように書き換えられる:

.. code-block:: haskell

  import Control.Monad.Reader
  import Control.Monad.IO.Class

  type App = ReaderT (Context (IORef Int)) IO

  sumByIORef :: [Int] -> App ()
  sumByIORef l = go l
    where
      go []     = pure ()
      go (x:xs) = do
        r <- subparam1 . subctx <$> ask
        liftIO $ modifyIORef' r \s -> s + x
        go xs

本来， ``State`` が補っていた部分を，読み込み部分は ``ReaderT`` に，可変部分は ``IORef`` と ``IO`` に任せる感じだ．このようなプログラミングスタイルは，何も僕が思いついたわけではなく， `ReaderT パターン <https://www.fpcomplete.com/blog/2017/06/readert-design-pattern>`_ と呼ばれていて，結構最近は浸透しつつあるんかな？ 今まで挙げたパフォーマンスの改善やコードの清潔さを担保するという他にも，このスタイルはメリットがあり，もうちょっと周辺のツールも整備されてたりするんだけど，まあ詳細は `元記事 <https://www.fpcomplete.com/blog/2017/06/readert-design-pattern>`_ の方を読んでくれ．(飽きてきた．)

まとめ
------

というわけで，状態更新を行う時の代表手法として紹介される State モナドだけど，Reader + IORef を使った方がいい場合もあるよという話でした．こういうパフォーマンス面での話は，誰も挙げていない気がしたので書いた感じ．

大雑把には，State モナドより Reader + IORef を使った方がいい場合として，状態が

* 大きくてネストしていたりというように，ほどほどに複雑で
* 局所的な変更が多くて (変更しない部分も多くて)

という条件を満たす時というのがある．この場合は，State よりも Reader + IORef の方がパフォーマンス面でもコードの簡潔さ的にも良い場合があるよという感じ．今回は，IORef しか紹介しなかったけど，これは TVar とかにも通じる話だし，STRef 使えば全体として純粋に計算できる場合もある．ま，そういう感じで (ざつぅ)．

.. [#notice-write-barrier] GHC では，GC の捕捉のため旧世代から新世代への参照が作られた場合の更新通知を，mutator が行う必要がある．この通知を `write barrier <https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/storage/gc/remembered-sets>`_ と呼んでいて，writeMutVar# も write barrier を内部で行う．しかし，write barrier があまりにも多いと，内部の仕組み的に GC の性能が下がるという問題が知られている．一般に，GHC の GC は可変なオブジェクトについてあまり良いサポートを提供できていないと言う `話 <https://gitlab.haskell.org/ghc/ghc/issues/7662>`__ もある．その意味では，純粋性を贔屓しているというのは正しい．
.. [#notice-optimization-0] 最適化レベル0，つまり最適化なしの場合，IORef の方が速くなる現象にはここまで触れなかったが，実はこれは State のせいというより mtl のせいという側面が大きい．普段私たちはそこまで意識していないのだが，実は型クラスを使うのにはそれなりの実行時コストがかかる．これらは，最適化によってそれなりに排除されている．しかし，最適化なしの場合はこのコストはもろに影響してくる．今回の場合は， ``IO`` モナドだけを使ったコードと比較し， ``State`` モナドのコードは mtl の API を使ったので ``Monad`` 型クラスと ``MonadState`` 型クラスの抽象化に依存している．つまり，その分コストが増えてしまったということになる．なので，最適化なしの場合は，あまり本質的な違いとは言えないだろう．
