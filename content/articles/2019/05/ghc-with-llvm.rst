GHC で LLVM を通したビルドを行う
================================

:date: 2019-05-17 15:01
:tags: Haskell, GHC, LLVM, macOS, Homebrew
:category: 環境構築

GHC で LLVM を通してビルドをしたかったので，設定した．その備忘録．

macOS で LLVM を GHC が使えるようにする
---------------------------------------

GHC が対応してる LLVM のバージョンは，標準で入ってるのよりちょっと古くって，さらにバージョンごとにサポートされてる範囲が違う．各 GHC でサポートされてる LLVM のバージョンは，以下に載ってる:

  https://gitlab.haskell.org/ghc/ghc/wikis/commentary/compiler/backends/llvm/installing#llvm-support

今回は GHC 8.6.5 を使ったので， LLVM のバージョンは 6 系になる．なので， LLVM 6 系を Homebrew でインストールする::

  brew install llvm@6

後は， GHC が見にいく llvm のツールの場所をインストールしたものに変更する． GHC は ``settings`` ファイルというものを持っている．これは単なるテキストファイルで， Haskell の ``[(String, String)]`` 型のダンプ情報の形式で書かれていて，ユーザが書き換え可能だ． GHC は実行するたびにこのファイルを見て， ``read :: String -> [(String, String)]`` した情報を元に設定を初期化する．このファイルは， ``lib`` ディレクトリのトップに置かれているのが通常で， ``ghcup`` を使っている場合， ``~/.ghcup/ghc/8.6.5/lib/ghc-8.6.5/settings`` にある．この設定の LLVM 関連部分を修正する．主に修正する箇所は 2 つ::

  [
  ...
  ("LLVM llc command", "llc"),
  ("LLVM opt command", "opt"),
  ("LLVM clang command", "clang")
  ]

のうちの ``LLVM llc command`` と ``LLVM opt command`` を LLVM 6 のパスに置き換える．置き換える場所は ::

  $ echo $(brew --prefix llvm@6)/bin/{llc,opt}
  /usr/local/opt/llvm@6/bin/llc /usr/local/opt/llvm@6/bin/opt

で，次のように書けばいい:

.. code-block:: diff

    [
    ...
  - ("LLVM llc command", "llc"),
  - ("LLVM opt command", "opt"),
  + ("LLVM llc command", "/usr/local/opt/llvm@6/bin/llc"),
  + ("LLVM opt command", "/usr/local/opt/llvm@6/bin/opt"),
    ("LLVM clang command", "clang")
    ]

LLVM clang command について
:::::::::::::::::::::::::::

``LLVM clang command`` はなんか変えなくても動くし，こいつを ``/usr/local/opt/llvm@6/bin/clang`` に変えてコンパイルすると以下のエラーが出てコンパイルができない::

  $ cabal new-build
  ...

  /var/folders/km/7h33fnqs7bddrz_xtx609ss00000gn/T/ghc78032_0/ghc_1.s:2:31: error:
      error: invalid OS update specifier, comma expected
              .build_version macos, 10, 14    sdk_version 10, 14
                                              ^
    |
  2 |         .build_version macos, 10, 14    sdk_version 10, 14
    |                               ^

  <no location info>: error:
      Error running clang! you need clang installed to use the LLVM backend
      (or GHC tried to execute clang incorrectly)
  `clang' failed in phase `Clang (Assembler)'. (Exit code: 1)

原因不明だが， ``LLVM clang command`` をいじらなければ普通に動くので，なんだろなって思って放置してる．原因誰か知ってたら教えてくださいって感じ．

LLVM を通してビルドする
-----------------------

後は， ``-fllvm`` オプションをつけてビルドすればいい． ``cabal`` の場合， ::

  cabal build --ghc-options=-fflvm

みたいにする．ついでに，ネイティブビルドの場合 ``-fasm`` を付ける．なお， LLVM の出力が見たかったら ``-ddump-llvm`` というオプションで見れるっぽい． LLVM 出力ができてるかどうかは， ``-ddump-llvm`` と ``-ddump-asm`` をつけて ``asm`` の出力が出てたら出来てない， ``llvm`` の出力が出てたら出来てるって感じで確認できる．

まとめ
------

macOS の環境の下， GHC で LLVM を使う方法を紹介した． LLVM 用のコード生成見る時とかに使っていきたい．後，パフォーマンスが劇的に改善するらしいね．

ただ， ``ghcup`` を使っている場合一々 ``settings`` を変更しなきゃいけないのは微妙なので，この辺なんとかしたいね．
