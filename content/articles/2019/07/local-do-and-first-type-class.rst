Local Do と型クラス
===================

:date: 2019-07-27 15:30
:tags: Haskell, GHC, GHC拡張, do構文, 型クラス
:category: プログラミング言語

現在， GHC に `線形型の提案 <https://github.com/ghc-proposals/ghc-proposals/pull/111>`_ がされていて活発に議論されている．プロトタイプも `ここ <https://github.com/tweag/ghc/tree/linear-types>`_ から利用可能だ． Docker イメージも利用可能なので，気軽に試せると思うので，ぜひ試してみてくれ．

さて，そんな線形型の提案の裏で，副産物として Local Do という拡張が提案されている．今回は，その拡張の紹介と最近思ってることのお話．

Local Do 拡張
-------------

Haskell の do 構文は，ご存知の通りモナドのメソッドを使った式を手続き型ライクに書ける構文だ:

.. image:: {attach}local-do-and-first-type-class/do-notation-syntax.png
  :alt: do 構文の要素
  :align: center

この構文は次の ``Prelude`` にある定義を想定している [#monad-class-in-ghc]_:

.. code-block:: haskell

  class Applicative m => Monad m where
    (>>=) :: m a -> (a -> m b) -> m b

  class Monad m => MonadFail m where
    fail :: String -> m a

  (>>) :: Applicative f => f a -> f b -> f b
  (>>) = (*>)

こいつらを元に，次の変換を行う:

.. image:: {attach}local-do-and-first-type-class/do-notation-translation.png
  :alt: do 構文の脱糖変換
  :align: center

この構文は ``Monad`` クラス専用だが，現在 Haskell には，

* `IxMonad <http://hackage.haskell.org/package/indexed-0.1.3/docs/Control-Monad-Indexed.html#t:IxMonad>`_ : indexed monad
* `Effect <https://hackage.haskell.org/package/effect-monad-0.8.1.0/docs/Control-Effect.html#t:Effect>`_ : graded monad
* `RMonad <https://hackage.haskell.org/package/rmonad-0.8.0.2/docs/Control-RMonad.html#t:RMonad>`_ : 制約付き monad

やそれを一般化した `super monad <https://hackage.haskell.org/package/supermonad>`_ などがあり，これらは全て型の異なる ``(>>=)`` / ``fail`` / ``(>>)`` を持つ．また， `線形型用の monad <https://github.com/tweag/linear-base/blob/0d6165fbd8ad84dd1574a36071f00a6137351637/src/Control/Monad/Linear.hs#L43>`_ なども考案されている:

.. code-block:: haskell

  class Applicative m => Monad m where
    (>>=) :: m a ->. (a ->. m b) ->. m b
    (>>) :: m () ->. m a ->. m a

  class Monad m => MonadFail m where
    fail :: String -> m a

これらは型は違うものの同じようなメソッドを持っており，同じように do 構文が適用できる．実際， indexed monad については， `indexed-do-notation <http://hackage.haskell.org/package/indexed-do-notation>`_ というパッケージで ``TemplateHaskell`` によっての実装が提供されている．また，線形型用の monad を提供する ``linear-base`` パッケージには， ``Control.Monad.Linear.Builder`` というモジュールが提供されており，その中では次のデータ構造が提供されている:

.. code-block:: haskell

  import qualified Control.Monad.Linear as Linear

  data BuilderType = Builder
    { (>>=)  :: forall m a b. Linear.Monad m => m a ->. (a ->. m b) ->. m b
    , (>>)   :: forall m b. Linear.Monad m => m () ->. m b ->. m b
    , fail   :: forall m a. Linear.MonadFail m => String -> m a
    , return :: forall m a. Linear.Monad m => a ->. m a
    }

  monadBuilder :: BuilderType
  monadBuilder = Builder
    { (>>=)  = (Linear.>>=)
    , (>>)   = (Linear.>>)
    , fail   = Linear.fail
    , return = Linear.return
    }

``Control.Monad.Linear`` は線形型用の monad を提供するモジュールだ．この ``Builder`` と ``RebindableSyntax`` / ``RecordWildCards`` 拡張を組み合わせて，以下のようなコードが書ける:

.. code-block:: haskell

  {-# LANGUAGE BlockArguments    #-}
  {-# LANGUAGE OverloadedStrings #-}
  {-# LANGUAGE RebindableSyntax  #-}
  {-# LANGUAGE RecordWildCards   #-}

  module Main where

  import           Prelude                      hiding (Monad(..), MonadFail(..))
  import           Prelude.Linear               (Unrestricted(..))
  import qualified Control.Monad.Linear.Builder as Linear
  import qualified System.IO                    as System
  import qualified System.IO.Resource           as RIO
  import           Data.String                  (fromString)

  main :: IO ()
  main = RIO.run $ writeSomething "something.txt"

  writeSomething :: FilePath -> RIO.RIO (Unrestricted ())
  writeSomething path = do
      h0 <- RIO.openFile path System.WriteMode
      h1 <- RIO.hPutStr h0 "some"
      h2 <- RIO.hPutStr h1 "thing"
      RIO.hClose h2
      return (Unrestricted ())
    where
      Linear.Builder{..} = Linear.monadBuilder

``RebindableSyntax`` は整数リテラルや do 構文などの糖衣構文において，中で使われるメソッドを， ``Prelude`` から探す代わりに現在のスコープから探すようにする拡張だ．なので，今回は通常の ``Monad`` のメソッドの代わりに， ``Linear.Builder{..} = Linear.monadBuilder`` で束縛したものが do 構文で使われる事になる．

``TemplateHaskell`` を使った例はコンパイル速度の問題と依存関係の問題がある． ``RebindableSyntax`` は ``NoImplicitPrelude`` に依存する他， do 構文以外の様々な構文に影響するという問題がある．そこで do 構文のみで内部で使用するメソッドを切り替えられるようにする拡張が， ``LocalDo`` 拡張になる．

現在の提案では， do 構文を拡張し，次のようなことが指定できるようにするという事になっている::

  do @Linear.monadBuilder
    h0 <- RIO.openFile path System.WriteMode
    h1 <- RIO.hPutStr h0 "some"
    h2 <- RIO.hPutStr h1 "thing"
    RIO.hClose h2
    return (Unrestricted ())

こう書くと，脱糖の際 ``Linear.monadBuilder`` のフィールドを使うようになる．

現在の提案の問題点
------------------

ところで，単純に脱糖の際 ``Linear.monadBuilder`` のフィールドを使うと言ったが，実際は幾つかのデザイン選択がある．例えば，次のプログラムを見てみよう::

  do @Linear.monadBuilder
    x <- m1 >> m2
    pure x

このプログラムはどう翻訳されるべきだろうか？ 例えば次の変換があり得るだろう::

  let
    Linear.Builder{..} = Linear.monadBuilder
  in (m1 >> m2) >>= \x -> pure x

このプログラムへの変換は，あなたが想定していたものだろうか？ この例では， do 構文中に出てきた ``>>`` も線形型版に置き換わる．これは一見便利だが，多くの場合意図しない挙動であり，しかもプログラム片を別の関数に切り出した場合動作が異なってしまう．これは言語デザインの一貫性の面からも微妙だ．現状の提案では，脱糖は実は Haskell の構文に則っておらず，上のような名前の衝突も起こすようなものになっている::

  do @b { x <- u; stmts }  =  case b of { >>= } -> (>>=) u $ \x -> do @b { stmts }
  do @b { u; stmts }       =  case b of { >> } -> (>>) u $ do @b { stmts }

``NameFieldPuns`` を前提に書かれてると提案では述べているが，実際にはこの拡張を有効にしても右側の構文はコンパイルが通らない．コンストラクタが省略されているためだ．読み方としては，単一コンストラクタのフィールドを束縛するのが，擬似構文 ``case v of { field } -> e`` ということになる．現在の提案は，これが問題視されているが，一意な解決案が出ていない．

解決案の一つとして， ``LocalDo`` のための以下のような ``Builder`` 型を作るという提案がなされている:

.. code-block:: haskell

  data Builder t_bind t_then t_fail = Builder
    { (>>=)  :: t_bind
    , (>>)   :: t_then
    , fail   :: t_fail
    }


  type MonadBuilder = Builder
    (forall m a b. Monad m => m a -> (a -> m b) -> m b)
    (forall m a b. Monad m => m a -> m b -> m b)
    (forall m a. MonadFail m => String -> m a)

  monadBuilder :: MonadBuilder
  monadBuilder = Builder (Prelude.>>=) (Prelude.>>) Prelude.fail

後は脱糖で使う関数を ``Builder`` のフィールドにすればいい．個人的にはこの提案が落とし所だと思う．一応触れられてる問題点としては，新たにフィールドを追加すると既存のコードが壊れやすいということだ．そのため，次のような形にすることも提案されている:

.. code-block:: haskell

  class HasBind b where
    type TypeOfBind b :: *
    localBind :: b -> TypeOfBind b

  class HasThen b where
    type TypeOfThen b :: *
    localThen :: b -> TypeOfThen b

  ...


  data MonadBuilder m = MonadBuilder

  instance Monad m => HasBind (MonadBuilder m) where
    type TypeOfBind (MonadBuilder m) = forall a b. m a -> (a -> m b) -> m b
    localBind MonadBuilder = (>>=)

  instance Monad m => HasThen (MonadBuilder m) where
    type TypeOfBind (MonadBuilder m) = forall a b. m a -> m b -> m b
    localBind MonadBuilder = (>>)

  ...

この場合，フィールドが増えても既存のコードを壊さないで拡張できるメリットがある．ただここまでくると，実は同じ機能を持ってる型クラス既にあるじゃんということで，次のようにすることが現在提案されている:

.. code-block:: haskell

  data MonadBuilder (m :: Type -> Type) = MonadBuilder

  type TypeOfMonadBind m = forall a b. m a -> (a -> m b) -> m b
  instance Monad m => HasField ">>=" (MonadBuilder m) (TypeOfMonadBind m) where
    getField MonadBuilder = (>>=)

  type TypeOfMonadThen m = forall a b. m a -> m b -> m b
  instance Monad m => HasThen (MonadBuilder m) (TypeOfMonadThen m) where
    getField MonadBuilder = (>>)

  ...

なお PureScript の方には，実は ``LocalDo`` 相当の機能が既に入っている． PureScript では qualified do と呼ばれていて，以下のように書ける:

.. code-block:: haskell

  import Prelude as P
  import Effect (Effect)
  import Effect.Console (log)

  main :: Effect Unit
  main = P.do
    log "Hello"
    log "World!"

qualified do はモジュールを指定して，そのモジュールの中から脱糖に必要な関数を探してくる．モジュールを切り替えることで DSL を様々なモジュールに着地させることができるわけだ．この方式でも名前空間の破壊は起こらないため，解決策の一つとして提案されている．ただ，これは実行時に着地先を変えることは出来ないため，表現能力が下がってしまう点が問題視されている．

また型解決についてあまり触れられていないのも，問題視されている．最悪脱糖してから型検査すればいいと思うが，かなりエラーメッセージが不親切になるだろう．また，脱糖してからだと，

.. code-block:: haskell

  do @builder
    putStr "Hello "
    putStr "World!"

と

.. code-block:: haskell

  do @builder
    _ <- putStr "Hello "
    putStr "World!"

で前者は ``(>>)`` だけ，後者は ``(>>=)`` だけを使うということになるが，片方は型エラーでもう片方はコンパイルに通るといったことになり，一貫性がない．そもそも， ``builder`` が全てのフィールドを兼ね揃えているかチェックすべきかどうかという点もある．また， ``builder`` の型をどう推論するか， ``LocalDo`` の文の型推論はどうするかについても考える必要があるだろう．

このように現状結構穴がある提案になっていて，提案者に対し修正が要求されている．また，積極的な議論が呼びかけられている．何かアイデアや疑問があれば，投げておくといいかもしれない．

Haskell の型クラスについて思うこと
----------------------------------

ところで少し話が変わるが， Haskell の型クラスは内部では単なるデータ型に変換される．例えば，

.. code-block:: haskell

  class SampleC a where
    sampleM1 :: a
    sampleM2 :: a -> a

  instance SampleC Int where
    sampleM1 = 0
    sampleM2 x = x + 1

  sampleV :: SampleC a => a
  sampleV = sampleM2 sampleM1

  sampleV2 :: Int
  sampleV2 = sampleV

みたいなコードがあった場合，内部的には

.. code-block:: haskell

  data DSampleC a = DSampleC
    { sampleM1 :: a
    , sampleM2 :: a -> a
    }

  dSampleC_Int :: DSampleC Int
  dSampleC_Int = DSampleC
    { sampleM1 = 0
    , sampleM2 = \x -> x + 1
    }

  sampleV :: DSampleC a -> a
  sampleV DSampleC{..} = sampleM2 sampleM1

  sampleV2 :: Int
  sampleV2 = sampleV dSampleC_Int

みたいなコードが生成される．型クラス相当の内部で定義されているデータ型を辞書と呼び，型クラスの制約が実際には辞書を渡していく形式に変換されることから，この変換を辞書渡しスタイルと呼ぶことがある．ところで，このように見てみると型クラスの仕組みとは，実際には辞書という暗黙のオブジェクトをパラメータとして持つ関数を定義する仕組みであるように見えてくる．そしてパラメータはコンパイル時にインスタンス解析によって暗黙的に埋め込まれる．今回の ``Builder`` 型は，実際には内部の辞書を顕在化させているにすぎないように見えてくる．もちろん，明示的にデータ型を指定できるので動的に作ったオブジェクトを指定することもできるわけだが，ほとんどの場合辞書を一回 Haskell で扱えるデータ型に変換して渡すことになるだろう．これは二重に手間だ．

Haskell の型クラスの実装を悪用した有名なパッケージとして， `reflection <https://hackage.haskell.org/package/reflection>`_ というものがある．これは一つしかメソッドを持たない型クラスが，内部で ``newtype`` 相当の実装をすることを利用し，暗黙の引数を型クラスの仕組みを通して実現するパッケージだ．これは，暗黙の引数以上のことを実現でき，辞書を一旦顕在化させてその後また型制約に戻すといったテクニックに利用できる．ただ，こちらも本来なら辞書をそのまま利用できれば解決できる話だ．

つまり Haskell の型クラスインスタンスが第1級でなく，そのためいくつかの場面で不便だということだ．もし第1級であれば，今回の提案ももう少し工夫の余地があっただろう． Scala はこれを実現していて，単に暗黙の引数として Scala のなんていうことはないクラスのインスタンスが渡されるので，それを通常の Scala プログラミングの範囲で取り扱える．同じように単に暗黙の引数とその解決のためのオブジェクトを簡潔に定義できる糖衣構文さえあれば，実は型クラスの仕組みは実現できる． Haskell の型クラスはかなり古参なので，いまさら変えようということになってもちょっと難しいと思うが，最近ちょっとそこら辺が不満で，今回の提案が入ってビルダを書く羽目になるたび，そういうこと考えるんだろなあというぼやきでした．

なお，こういうことを考えてる人は昔からちらほらいて，最近もそういう記事を目にした．まあ，みんなそう思うよねというお話でした．

まとめ
------

なんか最後ぐだぐだになったが， ``LocalDo`` 拡張の提案とその問題点，現在の議論の内容を紹介した．それから，型クラスについてちょっと不満に思っていることを書いた．

個人的にはこういう系は F# のコンピュテーション式が一番使い勝手良いなあと思っていたので，それ相当のことができる機能が GHC 拡張で提案されてるのは嬉しい．ただ，現状お蔵入りしかけなので，議論が活発になればいいなということで書いた．

後この記事は `Haskell-jpもくもく会 <https://haskell-jp.connpass.com/event/138061/>`_ の時間を使って書いたので宣伝をしとくと，月一ぐらいで Haskell に関していろんな人が集まって，1日個々で作業し成果を発表する会があって， Haskell-jp さんが主催してる．分からないことがあればすぐ隣の人とかに聞けるので，興味があれば参加してみるといいのではないでしょうか．

ということで，今日はこれで．

.. [#monad-class-in-ghc] GHC では ``Monad`` は， ``GHC.Base`` モジュールで定義されている．現状の GHC 8.6 ではこの記事で述べたものの他に ``return`` / ``(>>)`` / ``fail`` メソッドがある．これは， `MFP <https://prime.haskell.org/wiki/Libraries/Proposals/MonadFail>`_ / `MRP <https://gitlab.haskell.org/ghc/ghc/wikis/proposal/monad-of-no-return>`_ で削除される予定．この記事では， MFP / MRP が最終段階までいったことを想定した定義を書いている．
