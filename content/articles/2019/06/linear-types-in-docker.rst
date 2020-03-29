GHC の線形型プロトタイプのお試し環境を作る
==========================================

:tags: Haskell, 線形型, GHC, Docker
:category: プログラミング言語

現在， GHC に `線形型の追加提案 <https://github.com/ghc-proposals/ghc-proposals/pull/111>`_ が出ている．その提案に合わせて，プロトタイプも公開されている．内容はあまり詳しく追ってないけど，お試し環境は作ってみたのでその備忘録．

線形型プロトタイプ実装
----------------------

線形型の提案は， GHC Proposals (https://github.com/ghc-proposals/ghc-proposals/pull/111) で議論されている． https://arxiv.org/abs/1710.09756 の話を元にしているらしい．プロトタイプ実装は， Tweag の GHC フォーク (https://github.com/tweag/ghc) 内の ``linear-types`` ブランチで公開されている．

後，このプロトタイプを前提として，線形型用の ``base`` 代替パッケージ， ``linear-base`` パッケージ (https://github.com/tweag/linear-base) も公開されている．中には

* ``Prelude.Linear`` : ``Prelude`` の線形型版．色んな常用関数を線形型用に型を変えて提供してる．
* ``Control.Monad.Linear`` : 線形型版のモナドを提供してる．
* ``Control.Monad.Linear.Builder`` : 線形型版のモナドを ``RebindableSyntax`` によって do 構文で使えるようにするためのデータを提供する．
* ``System.IO.Linear`` : ``IO`` モナドを線形型を使って厳密に定義し直したやつを提供する．
* ``System.IO.Resource`` : リソース管理を線形型を使って型安全に行う仕組みを提供する．
* ``Unsafe.Linear`` : 線形型と普通の関数型のキャストを提供する．

みたいなモジュールがある (これが全部ではない)．

プロトタイプの利用は GHC のビルドが必要だが，時間が相当かかるため，公開されてる Docker イメージで Stack の連動機能を使うことが推奨されている．

お試し Docker 環境を作る
------------------------

ただ， Stack の連動機能を使わないで直接 Cabal を使って試したかったのでその環境を作った．公開されてる Docker イメージに， ``cabal-install`` を突っ込んで， ``linear-base`` パッケージを落としてきた状態にしただけだけど．

まず，次の ``Dockerfile`` を用意する:

.. code-block:: docker

  FROM tweag/linear-types:latest

  ENV CABAL_WORKDIR="/root"

  RUN apt-get update \
   && DEBIAN_FRONTEND=noninteractive apt-get install -y curl \
   && mkdir -p "${CABAL_WORKDIR}/.ghcup/bin" \
   && curl https://gitlab.haskell.org/haskell/ghcup/raw/master/ghcup > "${CABAL_WORKDIR}/.ghcup/bin/ghcup" \
   && chmod +x "${CABAL_WORKDIR}/.ghcup/bin/ghcup" \
   && "${CABAL_WORKDIR}/.ghcup/bin/ghcup" install-cabal \
   && apt-get remove -y curl \
   && apt-get autoremove -y \
   && rm -rf /var/lib/apt/*

  ENV PATH="${CABAL_WORKDIR}/.ghcup/bin:${CABAL_WORKDIR}/.cabal/bin:$PATH"

  ADD assets/Main.hs ${CABAL_WORKDIR}/Main.hs
  ADD assets/cabal.project ${CABAL_WORKDIR}/cabal.project
  ADD assets/playground.cabal ${CABAL_WORKDIR}/playground.cabal

  WORKDIR ${CABAL_WORKDIR}

  RUN mkdir -p src \
   && cabal new-update \
   && cabal new-build

  ENTRYPOINT ["cabal"]
  CMD ["new-repl"]

``tweag/linear-types`` が公開されてる Docker イメージで， Docker Hub に上がってる．こいつに ``ghcup`` で ``cabal-install`` を入れてるだけ．後適当に次のファイルも用意しとく::

  $ cat assets/Main.hs
  module Main where

  main :: IO ()
  main = putStrLn "Hello"
  $ cat assets/cabal.project
  source-repository-package
    type: git
    location: https://github.com/tweag/linear-base.git

  packages: ./
  $ cat assets/playground.cabal
  cabal-version: 2.4

  name: playground
  version: 0.0.0.1
  build-type: Simple

  common general
    autogen-modules:
      Paths_playground
    other-modules:
      Paths_playground
    build-depends:
      base >= 4.7 && < 5,

      linear-base
    ghc-options:
      -Wall
      -Wcompat
      -Wincomplete-uni-patterns
      -Wincomplete-record-updates
      -Wredundant-constraints
      -Wpartial-fields

      -fprint-explicit-foralls
      -fprint-potential-instances
      -frefinement-level-hole-fits=1

      -dcore-lint
    default-language: Haskell2010

  executable playground
    import: general
    hs-source-dirs: .
    main-is: Main.hs

後は，イメージをビルドして tty 有効で起動してやると， ``linear-base`` が入った環境で REPL で色々できる．適当に ``docker-compose.yml`` も作っておく::

  version: '3'

  services:
    playground:
      build: .
      volumes:
        - ./Main.hs:/root/Main.hs
      entrypoint: bash
      command: ["-c", "tail -f /var/log/lastlog"]

これで，カレントディレクトリに ``Main.hs`` を作って， Docker イメージ内でビルドしたり，読み込んだ状態で REPL 起動したりできる::

  docker-compose up -d
  docker-compose exec playground cabal new-build # ビルド
  docker-compose exec playground cabal new-repl  # REPL 起動

動作確認
--------

試しに，ちょっとしたサンプルを書いて動かしてみる．以下のようなサンプルを ``Main.hs`` として用意した:

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

こいつを REPL で動かしてみる::

  $ docker-compose exec playground cabal new-repl
  >>> :m Main
  >>> main
  >>> readFile "something.txt"
  "something"
  >>> :q

ちゃんと動いてそう．なお，フルセットは https://github.com/mizunashi-mana/docker-ghc-linear-types に上げた．

まとめ
------

線形型を試す環境を手に入れた．時間があったら色々試したいと思う．

``Prelude.Linear`` が色々 ``Prelude`` の記号と被せてきてるのは，正直微妙な気がする． ``const`` とかはともかく演算子は名前変えて欲しい感がある．
