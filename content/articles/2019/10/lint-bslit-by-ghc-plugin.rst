リテラルをコンパイラ時にチェックする
====================================

:tags: Haskell, GHC, リテラル, GHC拡張
:category: フレームワーク

Haskell はいくつかのリテラルで型クラスを使用したオーバーロードを許容している．例えば，Haskell で ``1`` と書いた場合，この型は ``1 :: Num a => a`` になり， ``Num`` のインスタンスを定義することで，リテラルに対する実体をユーザも制御できるようになっている．GHC拡張で文字列に対してのオーバーロードなども提供されている．ただ，提供するデータ型とリテラルによっては，プログラムが意図通りに動かなかったりクラッシュしてしまったりすることがある．これを事前にチェックする仕組みを，コンパイラプラグインで実装する方法を紹介する．

なお，環境として以下を想定している．

+--------------------+---------+
| GHC のバージョン   | 8.8.1   |
+--------------------+---------+
| Cabal のバージョン | 3.0.0.0 |
+--------------------+---------+

Haskell のオーバーロードリテラル
--------------------------------

Haskell では整数及び浮動小数点数リテラルにおいて，オーバーロードが許容されている．それぞれ，

* 整数リテラル: ``Num`` のインスタンス
* 浮動小数点数リテラル: ``Fractional`` のインスタンス

でオーバーロードされる．さらに， GHC 拡張で

* ``OverloadedStrings``: 文字列リテラルを ``IsString`` のインスタンスでオーバーロードする

基本的に，この実装は何か基盤となるデータ型に対し，そこからキャストするようになっている．例えば，整数リテラル ``1`` は ``fromInteger (1 :: Integer)`` に変換されている．リテラルそれぞれで変換の仕方を挙げると，

* 整数リテラル: ``Integer`` から ``fromInteger`` メソッドを使って変換
* 浮動小数点数リテラル: ``Rational`` から ``fromRational`` メソッドを使って変換
* 文字列リテラル: ``String`` から ``fromString`` メソッドを使って変換

のようになる．ただ，このキャストは必ずしも安全とは限らない．例えば， ``300 :: Data.Int.Int8`` というリテラルを考えると， ``300`` は 符号付き8bit 表現の域を超えてしまっている．実際に GHCi 上で試してみると，以下の出力が得られる::

  >>> 300 :: Data.Int.Int8

  <interactive>:1:1: warning: [-Woverflowed-literals]
      Literal 300 is out of the GHC.Int.Int8 range -128..127
  44

このような意図しない挙動を防ぐため，GHC では標準で提供するデータ型のリテラルに対しては上のようなオーバーフローチェックをしてくれる．ただ，標準で提供されてないデータ型については，このようなチェックは行われない．有名な例が ``ByteString`` に対する文字列リテラルのチェックだ． ``ByteString`` は利便性のため ``IsString`` のインスタンスを提供している．``ByteString`` は 8bit 文字の列で，バイト列の管理に使われる．ただ ``String`` は Unicode 文字の列で， ``ByteString`` は直接対応しない．そのため ``IsString`` インスタンスの実装は，特定のエンコードを決めてバイト列に変換するか，全ての文字が 8bit 内に収まっていると仮定しての変換の二通りということになる．``ByteString`` では後者が選ばれている．そのため，以下のようなことが起こる::

  >>> "あ" :: Data.ByteString.ByteString
  "B"

このような予期しない動作をするリテラルを，コンパイル時に判定して警告を出したいことは良くあるので，それを実装してみる．

GHC Compiler Plugin
-------------------

GHC にはコンパイルフェーズを挟み込めるプラグインシステムが存在する．型検査や Core-to-Core の変換が主だったが， 8.6 からは Source Plugin と呼ばれる構文解析や型検査後に環境を弄れるプラグインが作れるようになった．今回はこのうち，型検査後にフェーズを挟み込める機能を使用し，リテラルに対しての警告を表示する．

プラグインの作り方のドキュメントは， `GHCユーザガイド <https://downloads.haskell.org/~ghc/8.8.1/docs/html/users_guide/extending_ghc.html#compiler-plugins>`_ を参照すると良いだろう．また， mpickering 先生による `まとめサイト <https://mpickering.github.io/plugins.html>`__ も参考になると思う．基本的には， ``ghc`` パッケージの `GhcPlugins.Plugin <http://hackage.haskell.org/package/ghc-8.6.5/docs/Plugins.html#t:Plugin>`_ 型のデータを ``plugin`` という名前でエクスポートしたモジュールを作り，そのモジュールを ``-fplugin=<module>`` で指定してコンパイルを行えば良い．例えば，

.. code-block:: haskell

  module PluginExample where

  import qualified GhcPlugins

  plugin :: GhcPlugins.Plugin
  plugin = GhcPlugins.defaultPlugin

のようなモジュールを作り， ``-fplugin=PluginExample`` をオプションに加えてコンパイルすれば良い．型検査後にフェーズを挟み込む場合，次のように書く:

.. code-block:: haskell

  module PluginExample where

  import qualified GhcPlugins
  import qualified TcRnTypes   as TcM

  plugin :: GhcPlugins.Plugin
  plugin = GhcPlugins.defaultPlugin
    { GhcPlugins.typeCheckResultAction = pluginAction
    }

  pluginAction
    :: [GhcPlugins.CommandLineOption] -> GhcPlugins.ModSummary
    -> TcM.TcGblEnv -> TcM.TcM TcM.TcGblEnv
  pluginAction args mods tcEnv = doSomething

``doSomething`` の部分はデフォルトでは ``pure tcEnv`` で定義されている．それぞれの引数の内容は，

``args :: [CommandLineOption]``
  プラグインのコマンドライン引数． ``-fplugin`` オプションで渡せるようになっている．

``mods :: ModSummary``
  対象モジュールの概要

``tcEnv :: TcM.TcGblEnv``
  型情報の解析に必要なものがいろいろ入っている．

みたいな感じ．後，プラグインは再コンパイルの必要性などを指定できて，デフォルトでは強制的に再コンパイルされるようになっている．特に ``IO`` 処理に依存しないプラグインを書く場合は，次のようにしておくと良い:

.. code-block:: haskell

  module PluginExample where

  import qualified GhcPlugins
  import qualified TcRnTypes   as TcM

  plugin :: GhcPlugins.Plugin
  plugin = GhcPlugins.defaultPlugin
    { GhcPlugins.pluginRecompile = GhcPlugins.purePlugin
    , GhcPlugins.typeCheckResultAction = pluginAction
    }

デフォルトでは ``GhcPlugins.impurePlugin`` が指定されている．フラグが変化した際に再コンパイルする ``GhcPlugins.flagRecompile`` なども用意されている．

AST を再帰的に探索する
----------------------

では早速プラグインを作っていく．``TcGblEnv`` の ``tcg_binds`` フィールドには，トップレベルの束縛の型検査済の AST が入っている．まず，この AST を再帰的に辿ってリテラル部分を見つけていく．再帰的に式を辿る関数を書いてもいいが，今回は `SYB (Scrap Your Boilerplate) <http://hackage.haskell.org/package/syb>`_ を使う．こいつは， `Data <http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Data.html#t:Data>`_ のインスタンスであれば，再帰関数を動的な型キャストなどを使ってうまく実装し，提供してくれるパッケージだ．こいつを使って，以下のようにすると，簡単にリテラル部分を全探索するコードを書ける:

.. code-block:: haskell

  import qualified Generics.SYB as SYB

  import qualified HsExtension as HsSyn
  import qualified HsBinds     as HsSyn
  import qualified HsExpr      as HsSyn
  import qualified HsLit       as HsSyn

  pluginAction
    :: [GhcPlugins.CommandLineOption] -> GhcPlugins.ModSummary
    -> TcM.TcGblEnv -> TcM.TcM TcM.TcGblEnv
  pluginAction _args _mods tcEnv = do
    let lbinds = TcM.tcg_binds tcEnv
    lintLHsBinds lbinds
    pure tcEnv

  lintLHsBinds :: HsSyn.LHsBinds HsSyn.GhcTc -> TcM.TcM ()
  lintLHsBinds lbinds = sequence_ $ listify go lbinds
    where
      go (GhcPlugins.L loc expr) = case expr of
        HsSyn.HsWrap _ _ e  -> go $ GhcPlugins.L loc e
        HsSyn.HsOverLit _ l -> Just $ lintHsOverLit loc l
        _                   -> Nothing

  listify :: Typeable r => (r -> Maybe a) -> SYB.GenericQ [a]
  listify f = SYB.everything (++) $ [] `SYB.mkQ` \x -> toList $ f x

  lintHsOverLit :: GhcPlugins.SrcSpan -> HsSyn.HsOverLit HsSyn.GhcTc -> TcM.TcM ()
  lintHsOverLit loc lit = doSomething

``listify`` は与えられた ``Data`` のインスタンスのデータ型を再帰的に辿り，該当するデータ型の部分を見つけたら与えられた関数を適用し，適用結果をリストで集約して返す．これを使って， ``LHsExpr GhcTc`` の部分の探索を行なっている． ``LHsExpr GhcTc`` は式を表す型 ``HsExpr GhcTc`` にソースの位置情報がついたものになっている． ``HsExpr a`` のうち，リテラルに関するコンストラクタは次のものになる:

.. code-block:: haskell

  data HsExpr p
    = ...
    | HsLit (XLitE p) (HsLit p)
    | HsOverLit (XOverLitE p) (HsOverLit p)
    | ...

``HsLit`` はオーバーロードされていないリテラル，例えば unboxed なリテラルや ``OverloadedStrings`` 拡張のない環境下での文字列リテラルなどを表す． ``HsOverLit`` は逆にオーバーロードされたリテラルを表す． ``X~`` みたいな部分は `Trees that Grow <https://www.microsoft.com/en-us/research/publication/trees-that-grow/>`_ の実装部分．後からコンストラクタを拡張したり，フェーズによって情報を付加したりできるようになっている．まあ，だいたいは無視して良くて， ``HsOverLit`` コンストラクタの本体は ``HsOverLit p`` の部分．中身は，次のようになっている:

.. code-block:: haskell

  data HsOverLit p
    = OverLit {
      ol_ext :: XOverLit p,
      ol_val :: OverLitVal,
      ol_witness :: HsExpr p}
    | XOverLit (XXOverLit p)

``XOverLit`` コンストラクタの部分はやっぱり `Trees that Grow`_ の部分になっていて，現在はまだ使われていない．なので， ``OverLit`` コンストラクタが主要部分になる．それぞれのフィールドは，

``ol_ext``
  `Trees that Grow`_ の適用部分．型検査後はリテラルの型が入っている．

``ol_val``
  本体．リテラルの内容が入っている．

``ol_witness``
  脱糖先として想定される式の AST が入っている．例えば文字列リテラル ``"str"`` なら ``fromString "str"`` みたいなもの．

みたいな感じ． ``ol_val`` の中身は

.. code-block:: haskell

  data OverLitVal
    = HsIntegral   !IntegralLit
    | HsFractional !FractionalLit
    | HsIsString   !SourceText !FastString

とほぼそのままの形の物が入っている． ``FastString`` は GHC 内部で使用されている文字列を表すためのデータ型で，中身はちょっと情報を付加した ``ByteString`` になる．今回の場合は UTF-8 エンコードされたリテラルの文字列が入っている．

ByteString リテラルをチェックする
---------------------------------

後はこのリテラル情報を適当にチェックすれば良い．例えば， ``ByteString`` のリテラルをチェックしてみる．まず，概形は以下のようになる:

.. code-block:: haskell

  import Control.Monad

  lintHsOverLit :: GhcPlugins.SrcSpan -> HsSyn.HsOverLit HsSyn.GhcTc -> TcM.TcM ()
  lintHsOverLit loc lit = case lit of
      OverLit {
        HsSyn.ol_val = HsSyn.HsIsString _ l,
        HsSyn.ol_ext = HsSyn.OverLitTc _ ty} -> go l ty
      _ -> pure ()
    where
      go l ty = do
        b <- checkValidLiteral l ty
        unless b $ throwWarning l

      checkValidLiteral :: GhcPlugins.FastString -> GhcPlugins.Type -> TcM.TcM Bool
      checkValidLiteral l ty = doSomething

      throwWarning :: GhcPlugins.FastString -> TcM.TcM ()
      throwWarning l = doSomething

後は， ``ByteString`` の正常なリテラルかを判定する ``checkValidLiteral`` と，警告を発生させる ``throwWarning`` をそれぞれ実装していく． ``checkValidLiteral`` の概形は以下のようになる:

.. code-block:: haskell

  import qualified Data.Char as Char

  checkValidLiteral :: GhcPlugins.FastString -> GhcPlugins.Type -> TcM.TcM Bool
  checkValidLiteral l ty = case GhcPlugins.tyConAppTyCon_maybe ty of
      Nothing -> pure False
      Just tc -> do
        b <- isByteStringTyCon tc
        if b
          then pure isValidByteStringLiteral l
          else pure False
    where
      isByteStringTyCon :: GhcPlugins.TyCon -> TcM.TcM Bool
      isByteStringTyCon = doSomething

      isValidByteStringLiteral :: GhcPlugins.FastString -> Bool
      isValidByteStringLiteral l = all (\c -> Char.ord c < 256) $ GhcPlugins.unpackFS l

流れとしては，

1. リテラルの型を見て，その型が具体的な型でかつ
2.  ``ByteString`` の型コンストラクタで構築されている時，
3. 8bit の文字だけで構築されている場合

真を返すようにしている． ``isByteStringTyCon`` では ``ByteString`` の型コンストラクタかを判定する必要がある．この操作はちょっとめんどくさくて，以下のような実装になる:

.. code-block:: haskell

  import qualified TcRnMonad   as TcM
  import qualified Finder
  import qualified IfaceEnv
  import           Control.Monad.IO.Class

  isByteStringTyCon :: GhcPlugins.TyCon -> TcM.TcM Bool
  isByteStringTyCon tc = do
      tns <- findByteStringTyConNames
      pure $ elem (GhcPlugins.tyConName tyCon) tns
    where
      findByteStringTyConNames = traverse findByteStringTyConNameByModule
        [ "Data.ByteString.Internal"
        , "Data.ByteString.Lazy.Internal"
        ]

      findByteStringTyConNameByModule moduleName = do
        hscEnv <- TcM.getTopEnv
        let mn = GhcPlugins.mkModuleName moduleName
        fr <- liftIO $ Finder.findImportedModule hscEnv mn bytestringPackage
        case fr of
          Finder.Found _ md -> do
            tn <- IfaceEnv.lookupOrig md $ GhcPlugins.mkTcOcc "ByteString"
            pure $ Just tn
          _ -> pure Nothing

      bytestringPackage = Just $ GhcPlugins.fsLit "bytestring"

このような実装で，依存している ``bytestring`` パッケージのモジュールから， ``ByteString`` という名前だった型名のものをうまく探し出して，型コンストラクタの名前がその名前と一致するかをチェックすることができる．``TcM`` モナドは， ``MonadIO`` のインスタンスなので， ``IO`` 操作を普通に実行できる．なので，必要な操作が ``TcM`` の操作で見つからなかったら， ``IO`` の操作を探してみるか自分で実装するかすると良い．これで ``checkValidLiteral`` は実装できたので，後は警告を出す部分を実装する． ``throwWarning`` の実装は以下のようになる:

.. code-block:: haskell

  {-# LANGUAGE OverloadedStrings #-}

  import qualified Bag
  import qualified ErrUtils

  throwWarning :: GhcPlugins.FastString -> TcM.TcM ()
  throwWarning l = do
      dynFlags <- GhcPlugins.getDynFlags
      liftIO $ GhcPlugins.printOrThrowWarnings dynFlags $ warnMsgs dynFlags l
    where
      warnMsgs dynFlags l =
        let errDoc = ErrUtils.errDoc
              [ GhcPlugins.ftext $ "Literal \"" <> l <> "\" contains illegal characters for ByteString" ]
              [ ]
              [ GhcPlugins.text "Avoid to use non-8bit characters or may use Text instead."
              ]
            msg = ErrUtils.formatErrDoc dynFlags errDoc
            warnMsg = ErrUtils.mkPlainWarnMsg dynFlags loc msg
        in Bag.unitBag warnMsg

単純に警告を一つ作ってそれを投げてるだけ． ``Bag`` は GHC 内で使われている順序なしのコレクションで，内部は単なる二分木になっていて，挿入や結合が定数時間のデータ構造になっている．これで警告のコレクションを作って， ``printOrThrowWarnings`` に渡すと，警告の内容を見ていい感じに出力をしたりコンパイルを中断したりしてくれる．以上で実装できる．このプラグインを次のような例

.. code-block:: haskell

  {-# LANGUAGE OverloadedStrings #-}

  module Main where

  import qualified Data.ByteString.Char8      as Char8
  import qualified Data.ByteString.Lazy.Char8 as LazyChar8
  import qualified System.IO                  as System

  main :: IO ()
  main = do
      putBSStrLn "ascii string"
      putBSStrLn "のっとばいとすとりんぐ"
      putStrLn "すとりんぐ"
      putLBSStrLn "のっとれいじーばいとすとりんぐ"
    where
      putBSStrLn = Char8.hPutStrLn System.stdout
      putLBSStrLn = LazyChar8.hPutStrLn System.stdout

で動かしてみると，以下のような出力が得られる::

  Main.hs:12:16: warning:
      • Literal "のっとばいとすとりんぐ" contains illegal characters for ByteString
      • Avoid to use non-8bit characters or may use Text instead.
     |
  12 |     putBSStrLn "のっとばいとすとりんぐ"
     |                ^^^^^^^^^^^^^

  Main.hs:14:17: warning:
      • Literal "のっとれいじーばいとすとりんぐ" contains illegal characters for ByteString
      • Avoid to use non-8bit characters or may use Text instead.
     |
  14 |     putLBSStrLn "のっとれいじーばいとすとりんぐ"
     |                 ^^^^^^^^^^^^^^^^^

なお，解説のコードとはちょっと違うが， ``ByteString`` のリテラルをチェックするプラグインの全体は， https://github.com/mizunashi-mana/ghc-bslit-linter に上がっているので参考にして欲しい．

まとめ
------

GHC Compiler Plugin の新しく導入された Source Plugin を使って，リテラルをコンパイル時に検査する手法を紹介した．式を全部探索することになるので，でかいソースにかける場合コンパイル時間の増加が気になるところだが，一応こういうことができるよという感じ．ただ，プラグインを書くにはそれなりに GHC 内のノウハウを知る必要がある．

Source Plugin を使うと，初心者向けに警告メッセージを分かりやすくしたり増やしたりすることもできそうだ．メンテするのは大変かもしれないけど， SYB などを使えばそれなりに持続期間の長いプラグインは作れるかもしれない．
