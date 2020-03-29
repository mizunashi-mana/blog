GHC IO モナドの中身
===================

:tags: Haskell, GHC, IO
:category: プログラミング言語

Haskell の IO モナドって，中身どうなってたんだろと気になってて，ちょっと調べてみた．そのメモ．

IO モナドと RealWorld
---------------------

まずは基本から． IO モナドは， Haskell の根幹となるモナドで， ``main`` も IO で書く． GHC では中身は次のように定義されている [#io-monad-definition]_::

  -- In GHC.Types module of ghc-prim package
  newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))

見ての通り， ``IO a`` は ``State# RealWorld -> (# State# RealWorld, a #)`` の ``newtype`` だ．あんまり見慣れない ``State# RealWorld`` という型と ``(# , #)`` という型が出てきたけど， ``State# RealWorld`` の方は置いといて， ``(# , #)`` からまず説明しておく．

``(# , #)`` は特別に ``IO`` のみ許可されてる型というわけではなく， GHC の ``UnboxedTuples`` という拡張で提供される型．名前の通り， unboxed なタプルを提供してくれる．いわゆる多値というやつで，ヒープアロケーションされず，要素はレジスタかスタックに格納される．つまり， ``s -> (# s, a #)`` なのは特に性能とか実行時表現の違いを無視すれば， ``s -> (s, a)`` と書くのと同じ意味を持つ．なので， IO モナドとは型の定義だけ見れば， ``State (State# RealWorld)`` モナドと同じことを表現してることになる．

次に ``State# RealWorld`` とはなんなのかだけど， ``State#`` ， ``RealWorld`` 両方プリミティブで，言語表層的には ``ghc-prim`` パッケージの ``GHC.Prim`` モジュールで定義された，中身のいじれない型ということになる [#how-to-defined-primtypes]_ ．

GHC の IO モナドは，意味としては現実世界のコンテキストを表す ``State# RealWorld`` を状態とする State モナドだ．ただ，単なる State モナドではなく，線形使用制約というものがついている．つまり，現在の状態として受け取った ``State# RealWorld`` は，状態遷移において必ず 1 度だけ使わなければいけない．ただこの制約は， Haskell の型システムでは守られていることの保証ができない [#uniqueness-typing]_ ．そこで， GHC では IO に対する操作は隠蔽されている．そして， RTS が提供する IO 操作用のプリミティブ関数だけが ``State# RealWorld`` を弄るようになっている．例えば，以下の ``IORef`` を使用する IO モナドでのプログラム:

.. code-block:: haskell

  {-# LANGUAGE BlockArguments #-}

  import Control.Concurrent
  import Data.IORef

  main :: IO Bool
  main = do
    x <- newIORef True
    writeIORef x False
    readIORef x

は，プリミティブ命令と明示的な ``State# RealWorld`` の伝搬で書いてみると，以下のようになる [#ioref-use-mutvar-inside]_ :

.. code-block:: haskell

  {-# LANGUAGE BangPatterns  #-}
  {-# LANGUAGE MagicHash     #-}
  {-# LANGUAGE UnboxedTuples #-}

  import GHC.Base

  main :: IO Bool
  main = IO go
    where
      go :: State# RealWorld -> (# State# RealWorld, Bool #)
      go s0# =
        let
          !(# s1#, x# #) = newMutVar# True       s0#
          !(# s2#, () #) = writeMutVar# x# False s1#
          !(# s3#, r  #) = readMutVar# x#        s2#
        in (# s3#, r #)

State モナドで，返り値が unboxed tuple になり，状態が ``State# RealWorld`` に変わってるだけである．ランタイムは最終的にこの IO を受け取って，最初の ``State# RealWorld`` を渡して実行していく．まさに現実世界を状態として実行されていく純粋関数が， Haskell のプログラムというわけである．ただ，現実世界というのはままならないもので，パッと状態を元に戻すといったことはできないし，状態を複製するというわけにも，元のまま保存するというわけにもいかない．このような制約と整合性がつくようにするのが，線形使用制約である．実際にこの制約が破られるとどうなるか，少し見てみる:

.. code-block:: haskell

  {-# LANGUAGE BlockArguments #-}
  {-# LANGUAGE BangPatterns   #-}
  {-# LANGUAGE MagicHash      #-}
  {-# LANGUAGE UnboxedTuples  #-}

  import GHC.Base

  -- | Example of unsafe IO using
  --
  -- Some case:
  --
  -- >>> main
  -- World
  -- Hello
  --
  main :: IO ()
  main = IO \s0# ->
    let
      !(# _, () #) = unIO (putStrLn "Hello") s0#
      !(# _, () #) = unIO (putStrLn "World") s0#
    in (# s0#, () #)

``unIO`` は単に ``\(IO x) -> x`` で， ``GHC.Base`` で定義されている関数．上の関数は ``s0#`` を3回 (出力部分で2回，返り値で1回の計3回) 使用しており，また捨ててる状態もあるなど，やりたい放題なわけだけど，元の状態 ``s0#`` を返しているからといって何も出力されないわけでもないし，今回は想定と逆順とはいえ出力が行われているわけだが，最適化によっては片方しか出力がされないみたいなこともある．もちろん，エラーが起こった時に ``main`` を ``catch`` しても例外が取れるとも限らない．

現実世界は State モナドのように状態を気軽に ``get`` / ``put`` で差し戻したりすることはできない．実行した副作用を元に戻すことができないからだ． ``putStrLn "Hello"`` を一度計算として走らせてしまえば，それを後から差し戻そうとしてももう出力してしまった後かもしれない．そうなれば，戻す作業は大掛かりだしそもそもユーザが見てしまった出力をなかったことになんてできない．それができてしまったら，タイムマシンの発明だ．ノーベル賞が取れるだろう． ``IO`` はそのような事情を鑑みて，せめて実行タイミングを制御することを目的としたものだ．どの IO 処理の後に次の IO 処理を行うかを， ``State# RealWorld`` を線形使用制約に従って伝搬させることで保証させる．さらに IO 操作そのものは， ``State# RealWorld`` を渡してやらないと起動しない．これにより， IO 命令は第1級のデータとして扱えるわけだ． GHC のランタイムは， Haskell で扱えない ``State# RealWorld`` の実体を定義し，初期の現実世界を表す状態を作って，それをユーザが定義した ``main`` に渡してやるだけという感じになる．

IO コードの生成
---------------

ここからが本題．表面的には， IO モナドは以上の思想で運用されてるわけだけど，実際にこの線形な State モナドは GHC の内部でどのように扱われ，どのようなコード生成がされるんだろう？ それをいろいろ調査してみた．なお，使ったのは GHC 8.6.5 ．

まず， ``State# RealWorld`` の実際の値は，サイズ 0 ，つまり何の情報も持たず，レジスタもスタックも特に消費しない値として使われる．なので，意味的には上記で述べた通り現実世界を表現する仮想の状態として扱われるが，実行時に愚直にそのような状態更新が行われるわけでないということだ．実際に，まずは IO を使ったプログラムが，コンパイル過程でどのように変遷していくかを見てみる．対象は以下のプログラム:

.. code-block:: haskell

  import Data.IORef

  main :: IO Bool
  main = do
    x <- newIORef True
    writeIORef x False
    readIORef x

このプログラムの ``-ddump-simpl`` は，以下のようになる::

  main1
    :: GHC.Prim.State# GHC.Prim.RealWorld
      -> (# GHC.Prim.State# GHC.Prim.RealWorld, Bool #)
  main1
    = \ (s_i7hd :: GHC.Prim.State# GHC.Prim.RealWorld) ->
        case GHC.Prim.newMutVar#
              @ Bool @ GHC.Prim.RealWorld GHC.Types.True s_i7hd
        of
        { (# ipv_i7ho, ipv1_i7hp #) ->
        case GHC.Prim.writeMutVar#
              @ GHC.Prim.RealWorld @ Bool ipv1_i7hp GHC.Types.False ipv_i7ho
        of s2#_i7hT
        { __DEFAULT ->
        GHC.Prim.readMutVar# @ GHC.Prim.RealWorld @ Bool ipv1_i7hp s2#_i7hT
        }
        }

  main :: IO Bool
  main
    = main1
      `cast` (Sym (GHC.Types.N:IO[0] <Bool>_R)
              :: (GHC.Prim.State# GHC.Prim.RealWorld
                  -> (# GHC.Prim.State# GHC.Prim.RealWorld, Bool #))
                ~R# IO Bool)

余計な情報はかなり省いている． ``main1`` の部分が本体で， ``newtype`` が ``cast`` に変換され，インライン展開と最適化によって余計なコンストラクタが消えた結果，上の方で述べた ``State# RealWorld`` の明示的な伝搬におおよそ変換されている．後は， Core 特有の多相関数に対して型推論で推論された型の明示的な適用などが書かれている．ここらへんは， ``TypeApplications`` 拡張で書かれたコードだと思えば素直に読めると思う．最終的な Core はほぼこの形で出力される．次にこの Core は ``-ddump-stg`` で見ると，以下のように変換される::

  main1 =
    [] \r [void_0E]
      case newMutVar# [GHC.Types.True GHC.Prim.void#] of {
        Unit# ipv1_s7lS ->
            case
                writeMutVar# [ipv1_s7lS GHC.Types.False GHC.Prim.void#]
            of
            s2#_s7lT
            { (##) -> readMutVar# [ipv1_s7lS GHC.Prim.void#];
            };
      };

  main =
    [] \r [void_0E] main1 GHC.Prim.void#;

STG の大雑把な読み方だけど， ``[] \r [...] ...`` は関数を表すクロージャで最後の ``[...]`` に引数が並んでいて， ``f [...]`` は関数適用になる．なお， 1 引数の関数適用は ``[]`` が省略される．Core と対応させれば雰囲気はなんとなく分かるだろう．注目して欲しいのは， ``State# RealWorld`` の引数だった部分が，全て ``void_XX`` という変数名に置き換わっていること，適用部分も ``GHC.Prim.void#`` という値の適用に置き換わっていること，パターンマッチで unboxed tuple のマッチをしていた部分がそれぞれ ``Unit#`` / ``(##)`` でのマッチに置き換わっていることだ．これらの意味はこの後の C-- の出力を見れば分かるだろう． ``-ddump-cmm`` で C-- の出力を見ると，以下のようになっている::

  [main1_entry() //  []
          { info_tbls: [(c1y24,
                          label: main1_info
                          rep: HeapRep static { Fun {arity: 1 fun_type: ArgSpec 3} }
                          srt: Nothing)]
            stack_info: arg_space: 0 updfr_space: Nothing
          }
      {offset
        c1y24: // global
            if ((Sp + -8) < SpLim) (likely: False) goto c1y25; else goto c1y26;
        c1y25: // global
            R1 = main1_closure;
            call (stg_gc_fun)(R1) args: 8, res: 0, upd: 8;
        c1y26: // global
            I64[Sp - 8] = block_c1y1Y_info;
            R1 = GHC.Types.True_closure+2;
            Sp = Sp - 8;
            call stg_newMutVar#(R1) args: 8, res: 8, upd: 8;
      }
  },
  section ""data" . main1_closure" {
      main1_closure:
          const main1_info;
  },
  _c1y1Y() //  [R1]
          { info_tbls: [(c1y1Y,
                          label: block_c1y1Y_info
                          rep: StackRep []
                          srt: Nothing)]
            stack_info: arg_space: 0 updfr_space: Nothing
          }
      {offset
        c1y1Y: // global
            call MO_WriteBarrier();
            P64[R1 + 8] = GHC.Types.False_closure+1;
            call "ccall" arg hints:  [PtrHint,
                                      PtrHint]  result hints:  [] dirty_MUT_VAR(BaseReg, R1);
            R1 = P64[R1 + 8];
            Sp = Sp + 8;
            call (P64[Sp])(R1) args: 8, res: 0, upd: 8;
      }
  }]

  [main_entry() //  []
          { info_tbls: [(c1y34,
                          label: main_info
                          rep: HeapRep static { Fun {arity: 1 fun_type: ArgSpec 3} }
                          srt: Nothing)]
            stack_info: arg_space: 0 updfr_space: Nothing
          }
      {offset
        c1y34: // global
            call main1_info() args: 8, res: 0, upd: 8;
      }
  },
  section ""data" . main_closure" {
      main_closure:
          const main_info;
  }]

ちょっと長いけど，落ち着いて読めば大丈夫．まず， ``main_entry`` の部分は見た通り ``main1`` のクロージャを呼び出してるだけなのでいいだろう． STG で適用していたはずの ``GHC.Prim.void#`` や受け取っていた ``void_0E`` は完全に綺麗さっぱり消えていることがわかる．で， ``main1_entry`` の方だが， C-- を読むに当たって以下のパターンを覚えておくと読みやすいと思う．

スタックサイズチェック
  ::

    label_stackcheck: // global
            if ((Sp + -8) < SpLim) (likely: False) goto label_ok; else goto label_gc;
    label_gc: // global
            R1 = main1_closure;
            call (stg_gc_fun)(R1) args: 8, res: 0, upd: 8;
    label_ok: // global
            ...

  クロージャエントリの最初などには，スタックの上限チェックが挟まる． ``SpLim`` がスタックサイズの上限値を表していて，現在のスタックポインタがそれをオーバーしていないかチェックしている．スタックサイズが足りていないと，一旦 GC を走らせにいく．それでもオーバーするようなら stack overflow になる．

ヒープサイズチェック
  ::

    label_heapcheck: // global
        Hp = Hp + N;
        if (Hp > HpLim) (likely: False) goto label_gc; else goto label_ok;
    label_gc: // global
        HpAlloc = N;
        R1 = R1;
        call stg_gc_unpt_r1(R1) returns to label_heapcheck, args: 8, res: 8, upd: 8;
    label_ok: // global
        ...

  今回は出てこないが，こちらもよくあるやつなので覚えておいたほうがいいと思う．ヒープ領域へのアロケーション時にヒープの上限チェックが挟まる． ``HpLim`` がヒープサイズの上限値を表していて，確保する分 ( ``N`` ) 足したヒープポインタがそれをオーバーしていないかチェックする．ヒープサイズが足りてないと，一旦 GC を走らせにいく．それでもオーバーするようなら heap overflow になる．なお，呼ぶ GC の関数は確保する領域に置かれるオブジェクトによって異なる．

サンク評価
  ::

        if (R1 & 7 != 0) goto label_ok; else goto label_eval;
    label_eval: // global
        call (I64[R1])(R1) returns to label_ok
    label_ok: // global
        ...

  これも今回は出てないけど頻出のやつなので紹介しておく． GHC では pointer tagging という手法を使用していて，ポインタの下位 数 bit (うちは 64bit 環境でその場合 3 bit) はタグになっている [#heap-aligned]_ ．それぞれのタグの意味は https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/haskell-execution/pointer-tagging を参照してもらうとして，大事なのは 0 の場合サンクを表すということ．上のコードはサンクかどうかを判定し，サンクならそのクロージャの評価に移る．そして，その後コードを実行する．

STG でのプリミティブ命令の変換
  今回， STG で呼び出していた ``readMutVar#`` や ``writeMutVar#`` はどっかへ行ってしまっている．影を残しているのは ``newMutVar#`` のみでこいつも ``stg_newMutVar#`` という名前になっている．実はプリミティブ命令は全てが全て RTS に関数として登録されているわけではない．多くは C-- の命令列に翻訳される．翻訳のされ方を調べる方法は，以下のことをすればいい:

  1. プリミティブ命令のコンストラクタを調べる．

     `compiler/prelude/primops.txt.pp <https://gitlab.haskell.org/ghc/ghc/blob/ghc-8.6.5-release/compiler/prelude/primops.txt.pp>`_ を見にいく． ``primtype`` / ``primop`` が定義されている．名前の通り，プリミティブ型とプリミティブ命令を定義する部分． ``primop`` は， ::

       primop Constructor "identifier" Category Type
         {Description.}
         with attribute1 = value1 ...

     という書式で書かれてる．見たいプリミティブ命令の identifier を検索して，対応する Constructor をまず調べる．

  2. ``emitPrimOp`` の該当箇所を見にいく．

     `compiler/codeGen/StgCmmPrim.hs の emitPrimOp のパターンマッチ部分 <https://gitlab.haskell.org/ghc/ghc/blob/ghc-8.6.5-release/compiler/codeGen/StgCmmPrim.hs#L269>`_ を見にいく． ``emitPrimOp`` はプリミティブ命令のコンストラクタをパターンマッチして，対応する C-- のコードを出す．最初に調べたコンストラクタで，パターンマッチの該当部分を見ればどういうコードが出されるかが分かる．

  例えば， ``writeMutVar#`` は ::

    primop  WriteMutVarOp "writeMutVar#"  GenPrimOp
      MutVar# s a -> a -> State# s -> State# s
      {Write contents of {\tt MutVar\#}.}
      with
      -- See Note [Why MutVar# ops can't fail]
      has_side_effects = True
      code_size = { primOpCodeSizeForeignCall } -- for the write barrier

  と定義されている．なので対応するコンストラクタは ``WriteMutVarOp`` になる．こいつの生成箇所は， ::

    emitPrimOp dflags res@[] WriteMutVarOp [mutv,var]
      = do -- Without this write barrier, other CPUs may see this pointer before
            -- the writes for the closure it points to have occurred.
            emitPrimCall res MO_WriteBarrier []
            emitStore (cmmOffsetW dflags mutv (fixedHdrSizeW dflags)) var
            emitCCall
                    [{-no results-}]
                    (CmmLit (CmmLabel mkDirty_MUT_VAR_Label))
                    [(baseExpr, AddrHint), (mutv,AddrHint)]

  となっている．まさに， ``main1_entry`` の ::

    call MO_WriteBarrier();
    P64[R1 + 8] = GHC.Types.False_closure+1;
    call "ccall" arg hints:  [PtrHint,
                              PtrHint]  result hints:  [] dirty_MUT_VAR(BaseReg, R1);

  という部分が対応してる．なお，プリミティブ命令の一部は特殊で， ``out-of-line`` という属性を持ってたりする．今回は， ``newMutVar#`` がこの属性を持っている::

    primop  NewMutVarOp "newMutVar#" GenPrimOp
      a -> State# s -> (# State# s, MutVar# s a #)
      {Create {\tt MutVar\#} with specified initial value in specified state thread.}
      with
      out_of_line = True
      has_side_effects = True

  この属性を持っているやつに関しては， ``emitPrimOp`` では処理されず RTS に登録されている関数が使用される．その関数の中身は， `rts/PrimOps.cmm <https://gitlab.haskell.org/ghc/ghc/blob/ghc-8.6.5-release/rts/PrimOps.cmm>`_ で定義されている．中身の識別子は ``stg_`` という接頭辞をつけて Z エンコーディングされていて， ``newMutVar#`` は，以下の実装が対応する::

    stg_newMutVarzh ( gcptr init )
    {
        W_ mv;

        ALLOC_PRIM_P (SIZEOF_StgMutVar, stg_newMutVarzh, init);

        mv = Hp - SIZEOF_StgMutVar + WDS(1);
        SET_HDR(mv,stg_MUT_VAR_DIRTY_info,CCCS);
        StgMutVar_var(mv) = init;

        return (mv);
    }

大体この 4 点を押さえておけば，なんとなくで読めると思う．出力された C-- のコードがやってることは，大体以下の感じ:

1. スタックサイズチェック
2. 次の部分の継続をスタックに入れて， ``GHC.Types.True`` を引数に ``stg_newMutVar`` に移る．
3. ``stg_newMutVar`` から返ってきた ``MutVar`` に ``GHC.Types.False`` を書き込む．
4. ``MutVar`` から値を読み込む．
5. 読み込んだ値を返り値として，スタックから次の継続を実行する．

見ての通り，どこにも ``State# RealWorld`` は現れない．しかも， ``writeMutVar#`` の変換を部分を見れば分かるように，こいつは返り値が全く存在しない．ここから，

* ``Unit#`` は実行時表現を持たない (``newtype`` と同じ) ．
* ``(##)`` と ``GHC.Prim.void#`` は，完全に実行時表現を持たず，スタック・レジスタのいずれにも格納されない．

ということが分かると思う． ``State# RealWorld`` は STG の段階で実行時表現を持たない ``GHC.Prim.void#`` へと変換され， C-- の段階ではコード中に片鱗すら見せない形に変換される．

なお余談だけど， unboxed proxy と呼ばれる ``GHC.Prim.Proxy#`` 型も GHC では提供されていて，こいつは ``State#`` と同じく ``GHC.Prim.void#`` に変換され実行時には消えて無くなる．これを使って

.. code-block:: haskell

  type State# = Proxy#

と定義しても同じ操作が起きる． ``Proxy# a`` は unboxed なので，この型の値は関数で受け取る前に評価される::

  >>> import GHC.Prim
  >>> :set -XMagicHash
  >>> f :: Proxy# a -> (); f _ = ()
  >>> f undefined
  *** Exception: Prelude.undefined

これは ``State#`` でも同様．つまり， ``State# RealWorld`` は実行時に消えるからといって何の影響も実行に及ぼさないというわけではないし，なんら特別な仕組みで動いているわけでもないということ． IO モナドは， unboxed な線形使用のサイズを持たない型と unboxed tuple を使って表現されることにより，結果の値を取り出すためパターンマッチをすると必ず ``State# RealWorld`` は評価されるので，汎用的な unboxed の仕組みで評価タイミングを保証できることになる．なので，線形使用を守っているなら，特別な仕組みを必要とせず最適化やコード生成を行える．

Unsafe IO
---------

``State# RealWorld`` は直感的にはランタイムが内部で持っている外部情報を，仮想的に現実世界を表す状態とみたてた型で， ``IO`` の評価タイミングを保証するものだった．そして，実際のコード生成では完全に削除され，保証された評価順序での実行コードだけが生成される．実行コードは無引数の関数コードとして保存され，ランタイムがメインスレッドで GC を立ち上げた後この関数コードを呼び出す．これが一連の流れになる．

ただ，上の流れは線形使用制約を守った使い方をする場合の話だった．では，制約を守らない場合はどうなるのだろうか？ 上で紹介したコードについて同じように中間出力を見てみる:

.. code-block:: haskell

  {-# LANGUAGE BlockArguments #-}
  {-# LANGUAGE BangPatterns   #-}
  {-# LANGUAGE MagicHash      #-}
  {-# LANGUAGE UnboxedTuples  #-}

  import GHC.Base

  main :: IO ()
  main = IO \s0# ->
    let
      !(# _, () #) = unIO (putStrLn "Hello") s0#
      !(# _, () #) = unIO (putStrLn "World") s0#
    in (# s0#, () #)

これは STG の段階では以下のようなコードになっていた::

  main1 =
      [] \r [void_0E]
        case
            GHC.IO.Handle.Text.hPutStr'
                GHC.IO.Handle.FD.stdout
                main4
                GHC.Types.True
                GHC.Prim.void#
        of
        { Unit# ipv1_sRHj [Occ=Once!] ->
              case ipv1_sRHj of {
                () ->
                    case
                        GHC.IO.Handle.Text.hPutStr'
                            GHC.IO.Handle.FD.stdout
                            main2
                            GHC.Types.True
                            GHC.Prim.void#
                    of
                    { Unit# ipv3_sRHn [Occ=Once!] ->
                          case ipv3_sRHn of { () -> Unit# [GHC.Tuple.()]; };
                    };
              };
        };

色々省略してるが，なんとなく出されたコードは分かるだろう． ``main4`` は ``"World"`` を表す文字列データ， ``main2`` は ``"Hello"`` を表す文字列データが格納されている． ``GHC.IO.Handle.Text.hPutStr' :: Handle -> String -> Bool -> IO ()`` は ``putStrLn`` が最終的に参照する ``base`` パッケージの関数で， 3 番目の引数に ``True`` を渡すと，改行込みで文字列を出力する． ``State# RealWorld`` を渡している部分は全て ``GHC.Prim.void#`` に置き換わっており，返り値にいたっては ``Unit# [GHC.Tuple.()]`` を返すなどまるっきりの無視であり，制約を守らなくても特に特別な処理はされずやはり ``State# RealWorld`` は実行時には完全に消えることが示唆されている．ただ，注目したいのは ``hPutStr'`` の呼び出される順序で， ``"World"`` が渡された後 ``"Hello"`` が渡されていることが分かる．これが， ``let`` で書いた順とは逆順に文字列が出力される原因である．

Haskell の仕様では let と case ， BangPattern のセマンティクス [#let-semantics]_ [#case-semantics]_ [#bangpattern-semantics]_ として，上から書いた順に評価されるのが基本だが，これらは副作用がないことが前提になっているので，最適化次第で順序を変えることができる．今回は逆順になっている．なお，明示的に以下のように ``let`` を分けて書くとちゃんとした順序になる:

.. code-block:: haskell

  main :: IO ()
  main = IO \s0# ->
    let
      !(# _, () #) = unIO (putStrLn "Hello") s0#
    in let
      !(# _, () #) = unIO (putStrLn "World") s0#
    in (# s0#, () #)

ただこれももちろん最適化次第．線形使用制約を満たさないコードを書いても大体は直感的に動くけど，このように最適化やコード生成に影響する部分だと，直感に反する挙動をする場合がある．

さて， IO の動作は制約を守った上で，プログラムが操作順序をきちんと制御しながら書くのが理想だが，現実的には少し制約を破ってもパフォーマンスの改善やプログラミングを容易にしたいことがある．そのため， GHC ではこの制約を破る方法もいくつか提供されている．これが， ``GHC.IO.Unsafe`` モジュールで提供されている関数群．主要なのは，

.. code-block:: haskell

  unsafeDupablePerformIO  :: IO a -> a
  unsafeDupablePerformIO (IO m) = case runRW# m of (# _, a #) -> a

  unsafeDupableInterleaveIO :: IO a -> IO a
  unsafeDupableInterleaveIO (IO m)
    = IO ( \ s -> let
                    r = case m s of (# _, res #) -> res
                  in
                  (# s, r #))

の 2 つの関数． ``runRW# :: (State# RealWorld -> a) -> a`` は ``ghc-prim`` パッケージの ``GHC.Magic`` モジュールで定義されてる関数で，適当に ``State# RealWorld`` を適用する関数． ``GHC.Prim`` モジュールに ``realWorld# :: State# RealWorld`` という値が定義されていて， ``runRW# f = f realWorld#`` で定義されている．もちろん， ``realWorld# = GHC.Prim.void#`` だ．

``unsafeDupablePerformIO`` は， IO に ``State# RealWorld`` を無理やり適用して結果を抽出する．つまり，副作用が起こる操作を無理やり実行するわけだ． ``unsafeDupableInterleaveIO`` は ``State# RealWorld`` を受け取ったもので適用するが，結果の状態は無視して元の状態を返す． ``r`` はサンクになっていて， ``r`` が評価された際初めて IO 操作が現在の IO 操作順序とは無関係に動き始める．特別な処理をしているわけではないが，感覚的には割り込みみたいなものだと思って良いと思う．

これら 2 つの関数は dupable と付いてる通り，マルチスレッドの場合一回だけ実行されるとは限らない．通常の副作用のない計算もそうだが，マルチスレッドの場合サンクはそれぞれのスレッドで評価される可能性がある．なお，マルチスレッドにおいてサンク評価をシングルスレッドに制限する方法も用意されていて， ``GHC.Prim.noDuplicate# :: State# RealWorld -> State# RealWorld`` がそう．こいつの本体は， ``PrimOps.cmm`` の ``stg_noDuplicatezh`` で，説明が https://gitlab.haskell.org/ghc/ghc/blob/ghc-8.6.5-release/rts/PrimOps.cmm#L2188 に書かれている．要点は，あるスレッドで ``noDuplicate#`` が実行され，さらにもう一つのスレッドでも実行された場合もう一つの方はブロックされるということ．これを使って，通常使う API は定義されている:

.. code-block:: haskell

  unsafePerformIO :: IO a -> a
  unsafePerformIO m = unsafeDupablePerformIO (noDuplicate >> m)

  unsafeInterleaveIO :: IO a -> IO a
  unsafeInterleaveIO m = unsafeDupableInterleaveIO (noDuplicate >> m)

  noDuplicate :: IO ()
  noDuplicate = IO $ \s -> case noDuplicate# s of s' -> (# s', () #)

これにより，評価が始まると ``noDuplicate`` が実行され単一スレッドでのみ評価が行われることになる． ``unsafePerformIO`` は副作用をないものとして評価する関数で ``Debug.Trace.trace`` で使われている． ``unsafeInterleaveIO`` は ``hGetContents`` など遅延 IO で使われていて， IO の皮を被っておきながら実際の操作は結果のサンクを評価した時に初めて実行されるというものだ．通常の IO はただの State モナドなので， IO の文脈で使われている限りインライン化しても問題ないわけだけだが， ``unsafePerformIO`` は ``State# RealWorld`` を適用して文脈外に値を取り出してしまうので，どこで評価が起きるかも保証されないし，内容によってはインライン展開のされ方によって副作用が何回か起きたりする場合もある． ``Debug.Trace.trace`` を使ったことがある人は分かると思うが，文字列が出力されるタイミングは予測がかなり難しいし，インライン展開のされ方によって複数回出力がある場合もあればはじめの一回のみといったこともあり得る．

このように ``State# RealWorld`` の線形使用を無視すると，便利なこともある反面，気をつけなければいけないことがかなり増える．副作用を持つ関数を遅延評価で扱うのはかなり骨が折れるし，その中で順序の保証を行えるようなデータ構造を与えているのに，そのデータ構造を無視する使い方をしてるわけだから当たり前といったらそうなのだけど．

とにかく， IO 自体は特別な仕組みを特に提供しておらず， unsafe な場合も例外ではない．特別な仕組みを提供する場合も ``noDuplicate`` のようにプリミティブ操作を与えることで制御を行なっていることが分かる．

ST モナドと IO モナド
---------------------

IO モナドとよく一緒に語られるものとして ST モナドがある．こいつは，以下の定義になっている [#st-monad-definition]_::

  newtype ST s a = ST (State# s -> (# State# s, a #))

IO と違うところは ``State# RealWorld`` が ``State# s`` と多相的になってるだけ．なので， ``IO`` とは， ``ST RealWorld`` のことだ．実際 ``stToIO :: ST RealWorld a -> IO a`` という命令も提供されている．

ST モナドは， ``s`` が多相的なままで使われる． ``GHC.Prim`` モジュールで提供されている API には ``State# RealWorld`` を扱うものと ``s`` を型パラメータとして ``State# s`` を扱うものの 2 種類が提供されており，例えば ``newMutVar#`` や ``newArray#`` などは多相的なまま扱える．多くの場合重い命令やインライン展開されるとまずい命令などは ``State# RealWorld`` で扱われ， ``MutVar`` や ``Array`` を扱うような軽く IO 処理が局所的に閉じられるような命令は多相的になっており，多相的になっている方が ``ST`` モナドで使用できる．もちろん，多相パラメータを ``RealWorld`` に特殊化してやれば IO と同様の操作もできる．

これらの多相的なプリミティブ命令のラッパとして， ``Data.STRef`` や ``array`` パッケージの ``Data.Array.ST`` などが提供されており， ``IORef`` や ``IOArray`` は内部でこの ``ST`` モナドの元で提供されてる API を ``RealWorld`` で特殊化してさらにラップしてる．

ST モナドは多相的なまま保っている，つまり局所的に閉じられる処理のみ使っている場合は ``runST :: (forall s. ST s a) -> a`` で純粋な計算の中で計算可能だ．こいつは遅延しようがインライン展開されようが同じ処理を行い，現実世界にも見た目上特に影響を与えず [#runst-is-pure]_ ，実行される． ``RealWorld`` で特殊化された処理も入っている場合は ``stToIO`` で IO に変換できる． ``runST`` の実装がどうなってるかだが，次のようになっている::

  runST :: (forall s. ST s a) -> a
  runST (ST st_rep) = case runRW# st_rep of (# _, a #) -> a

ところで ``GHC.Prim`` で定義されているデータ型の中にはパラメータに ``s`` があるものもある． ``runST`` では結果の型 ``a`` は ``s`` に依存しない必要があるので，このような型の値は ``runST`` では外に出せないようになっている．例えば， ``MutableByteArray# s`` は外に出せない．もしこいつが外に出せてしまったとすると， ::

  arr :: MutableByteArray# s
  arr = runST $ ST \s# -> newByteArray# 10 s#

  update0Arr :: Int -> Int
  update0Arr (I# i) = runST $ ST \s0# ->
    let
      !(# s1#, res# #) = readIntArray# arr 0# s0#
      !s2# = writeIntArray# arr 0# i s1#
    in (# s2#, I# res# #)

というプログラム (このプログラムは実際にはコンパイルエラーになる) で ``update0Arr`` は前回書き込んだ値を返し，パラメータによって違う値を返してしまうことになる．このようなことを防ぐため，外に出してはいけないデータ型はパラメータ ``s`` が付いている．このように， ST も IO と同じように特別なことはしておらず，既存の汎用的な仕組みを利用して，うまく実装されている [#runrw-magic]_ ．もちろん，コード生成も IO と同じように ST のラッパーが消え去って， ``State# s`` も消え去ることになり，後は伝搬された ``State# s`` の順序に従って評価順序が決定される．

まとめ
------

GHC の IO についてちょっと調べた．実は， IOManager と RTS の内部の説明に行き着きたかったんだけど，長くなりそうなんで別途にすることにする．

なんとなくここら辺は知っていたんだけど，ここまで実際にコード生成見てみたことはなかったんで，色々知識が補強された．この辺結構賢いよなあって毎回思う．僕もこういうやつパッと思いつけるぐらい，強くなりてえなあ．

.. [#io-monad-definition] http://hackage.haskell.org/package/ghc-prim-0.5.3/docs/src/GHC.Types.html#IO
.. [#how-to-defined-primtypes] なお， ``GHC.Prim`` の Haddock ドキュメントは全てダミー定義になっていて， https://gitlab.haskell.org/ghc/ghc/blob/ghc-8.6.5-release/compiler/prelude/primops.txt.pp から生成されている．
.. [#uniqueness-typing] 線形使用制約は，一意性とも呼ばれている．この制約を保証できる型システムを持ってる言語も，世の中にはいくつかある． Clean が有名だけど，最近だと Idris なども持っていて，一意型と言われる特殊な型を導入して，この型に関する検査で制約を検査する．
.. [#ioref-use-mutvar-inside] ``IORef`` は中で ``MutVar#`` を使用している．本来なら，変換前のプログラムと完全に合わせるには，適宜コンストラクタで ``MutVar#`` を包む操作が必要だが，ここではその部分は省略している．
.. [#heap-aligned] なお， ``+ 8`` をスタックポインタにしている箇所が多いのはこのためで，ポインタはタグのため 8 byte ごとにしか番地を表せない．そのため，ヒープ領域もこのバイト数を単位に整地されていて， ``+ 8`` は次の番地という意味になる．
.. [#let-semantics] https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-450003.12
.. [#case-semantics] https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-610022
.. [#bangpattern-semantics] https://downloads.haskell.org/~ghc/8.6.5/docs/html/users_guide/glasgow_exts.html#recursive-and-polymorphic-let-bindings
.. [#st-monad-definition] https://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.ST.html#ST
.. [#runst-is-pure] もちろん，スタックやヒープをいじることにはなるし， GC のお世話にもなるわけだけど，それは純粋な計算の場合も同じだ．
.. [#runrw-magic] なお， https://gitlab.haskell.org/ghc/ghc/blob/ghc-8.6.5-release/compiler/coreSyn/CorePrep.hs#L948 に書いてある通り，実は最適化によっては外に出てはいけないものが ``runST`` の外に出てしまうことがある．なので，実際には ``runST`` ，もとい ``runRW#`` を最適化において特別扱いする必要がある．具体的には，インライン展開されるタイミングを他のものと比べずらしてるらしい．
