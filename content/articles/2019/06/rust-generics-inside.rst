Rust のジェネリック関数はどうコンパイルされるのか
=================================================

:date: 2019-06-24 18:00
:tags: Rust, 多相関数, ジェネリクス, トレイト
:category: プログラミング言語

最近， (特に境界条件を伴った) 多相関数が言語ごとにどういうコードを吐くかが気になったので， Haskell とコードの生成方針が異なるという噂の Rust を見てみることにした．その覚え書き．なお， Rust 普段使いではないので間違ってるかもしれないのと，情報が古いかもしれない．

https://rust-lang.github.io/rustc-guide/about-this-guide.html が主な参考文献．使った ``rustc`` は，

* rustc: ``1.37.0-nightly (4edff843d 2019-06-16)``
* cargo: ``1.37.0-nightly (545f35425 2019-05-23)``

で， macOS で試してる．

Rust のコンパイルフロー
-----------------------

まず， Rust のコンパイルフローとその情報を見る方法について，確認する． Rust は大体以下の流れでコード生成をするっぽい．

1. ソースコードをパースして， AST に．
2. 名前解決 / マクロ展開を行う．
3. AST を HIR (構文をより単純にしたもの) に変換．
4. 型検査などの意味解析を行う．
5. HIR を MIR (借用の解析に適した中間言語) に変換．
6. 借用検査及び最適化を行う [#mir-based-borrow-check]_ ．
7. MIR を LLVM IR に変換する．
8. LLVM でオブジェクトファイルを生成させる．
9. オブジェクトファイルを lld でリンクする．

それぞれの中間言語を簡単に紹介しておくと，まず HIR は Rust の構文をやや削って単純化したもので，

* 演算子の優先順位の概念はなく，括弧が必ずつく．
* ``for`` や ``if let`` といった構文がない．

という感じになっている． ``for`` や ``if let`` は， ``loop`` や ``match`` ， ``let`` などを使って表現される．また，存在型が明確に入っていて，戻り値の型で使われる ``impl Trait`` がこの型に変換されるらしい．ソースコードに対応する HIR を見たければ， nightly のみで使える ``Z`` オプションを使って以下のようにする::

  rustc -Zunpretty=hir source.rs

なお，内部で扱う生の木構造を見たかったら ``hir-tree`` ，型解決された後の型が明示されたものを見たかったら ``hir,typed`` を ``-Zunpretty`` に渡せばいいっぽい．

MIR は，コード生成に寄せ，最適化のため制御フローグラフが明示された中間言語．制御フローグラフの各ノードを，ブロックとして囲んで， ``goto`` 文や他幾つかのプリミティブ命令を取り入れ，ブロックからブロックの遷移をそれで表している．後，変数の使用範囲や ``move`` なども明示されてる．こいつを表示したかったら以下のようにする::

  rustc -Zunpretty=mir source.rs

Graphviz 向けに DOT 言語を使ってグラフの形で出力してくれるやつもあって， ``mir-cfg`` を ``-Zunpretty`` に渡すとそういう表示になる．

LLVM IR はまあ勝手知ったるものだと思うので，まあそういうやつです．以下のようにして出力ができる::

  rustc --emit=llvm-ir -Ccodegen-units=1 source.rs

これを行うと ``source.ll`` というファイルにコードが生成される．なお， ``cargo`` を使う場合， ``cargo-asm`` プラグインを使うのが良いっぽい．こいつを使う場合， ::

  cargo llvm-ir

で， LLVM IR での関数単位を表示してくれ， ::

  cargo llvm-ir func

で， ``func`` のコードが標準出力に表示できる．なお，デフォルトで表示されるのはリリースビルドになっているので，最適化があまりかかっていないものを見たかったら， ::

  cargo llvm-ir --build-type=debug func

と指定するのが良さそう．

オブジェクトファイルも LLVM IR と同じように ``--emit=obj`` で出力でき， ``source.o`` みたいなファイルが生成される．

なお， Rust はライブラリを ``crate`` という単位で管理する．通常ビルドする時は，この ``crate`` ごとにスタティックライブラリのアーカイブである ``rlib`` ファイルを作る． ``rlib`` ファイルは，

* オブジェクトファイル (``*.o``)
* LLVM のビットコード (``*.bc.z``)
* Rust 独自のメタデータ (``rust.metadata.bin``)

を含む ``ar`` アーカイブになっている． ``cargo`` でビルドする場合は， ``target/{debug,release}/deps/*.rlib`` に依存 ``crate`` のライブラリが置かれるっぽい．で，メタデータの部分の解析が今回メインになるんだけど，こいつは単体でも出力できて， ``--emit=metadata`` とやると ``libsource.rmeta`` みたいなファイルが生成される．

Rust のジェネリック関数のコード生成アプローチ
---------------------------------------------

Rust のジェネリック関数のコード生成方法の概要は， Rust Book の `Performance of Code Using Generics <https://doc.rust-lang.org/1.35.0/book/ch10-01-syntax.html#performance-of-code-using-generics>`_ で

  Rust accomplishes this by performing monomorphization of the code that is using generics at compile time. Monomorphization is the process of turning generic code into specific code by filling in the concrete types that are used when compiled.

  In this process, the compiler does the opposite of the steps we used to create the generic function in Listing 10-5: the compiler looks at all the places where generic code is called and generates code for the concrete types the generic code is called with.

と述べられている通り，

1. ジェネリック関数が使われている部分を全て探し出して，
2. その中から具象型で使われてる部分を抽出し，どの具象型で使われてるかを洗い出し，
3. それぞれの具象型に対して単相的なコードを埋め込み，
4. 使われてる部分のジェネリック関数の呼び出しを埋め込んだ単相的な関数の呼び出しにすげ替える．

つまり， C++ のテンプレートと同じアプローチを取る．もちろん単相的な関数を生成する為には，元のジェネリック関数の実装を知っている必要がある．ただ， Rust には C++ のようにヘッダと実装を分ける機能はない．なので，ジェネリック関数が ``crate`` を超えて使われる場合，各 ``crate`` は単にリンクするようのオブジェクトファイルを提供するだけでなく，自身が持っているジェネリック関数の実装をメタデータとして渡してやる必要がある．ついでに，渡してやる必要のあるデータは他にも幾つかある．詳細は後で見る．

とりあえず，本当に単相的なコードを埋め込んでいるかのちょっとした確認をしておく．次のようなコードを用意する::

  // monomorphization.rs

  pub fn generic_func<T>(x: T) -> T {
    x
  }

  pub fn test_func1() -> i32 {
    generic_func(0)
  }

  pub fn test_func2() -> i32 {
    generic_func(333)
  }

  pub fn test_func3() -> Option<i32> {
    generic_func(None)
  }

こいつに対応する LLVM IR を見てみる::

  $ rustc --emit=llvm-ir -Ccodegen-units=1 --crate-type=lib monomorphization.rs
  $ cat monomorphization.ll
  ...

  ; monomorphization::generic_func
  ; Function Attrs: uwtable
  define i32 @_ZN16monomorphization12generic_func17h4e8466f957c62c4dE(i32 %x) unnamed_addr #0 {
  start:
    ret i32 %x
  }

  ; monomorphization::generic_func
  ; Function Attrs: uwtable
  define { i32, i32 } @_ZN16monomorphization12generic_func17hdce2324413889031E(i32 %x.0, i32 %x.1) unnamed_addr #0 {
  start:
    %0 = insertvalue { i32, i32 } undef, i32 %x.0, 0
    %1 = insertvalue { i32, i32 } %0, i32 %x.1, 1
    ret { i32, i32 } %1
  }

  ; monomorphization::test_func1
  ; Function Attrs: uwtable
  define i32 @_ZN16monomorphization10test_func117hd71db534fc9d7a5fE() unnamed_addr #0 {
  start:
  ; call monomorphization::generic_func
    %0 = call i32 @_ZN16monomorphization12generic_func17h4e8466f957c62c4dE(i32 0)
    br label %bb1

  bb1:                                              ; preds = %start
    ret i32 %0
  }

  ; monomorphization::test_func2
  ; Function Attrs: uwtable
  define i32 @_ZN16monomorphization10test_func217h14aba5cafddf6defE() unnamed_addr #0 {
  start:
  ; call monomorphization::generic_func
    %0 = call i32 @_ZN16monomorphization12generic_func17h4e8466f957c62c4dE(i32 333)
    br label %bb1

  bb1:                                              ; preds = %start
    ret i32 %0
  }

  ; monomorphization::test_func3
  ; Function Attrs: uwtable
  define { i32, i32 } @_ZN16monomorphization10test_func317h7ff127b737116f48E() unnamed_addr #0 {
  start:
    %_1 = alloca { i32, i32 }, align 4
    %0 = bitcast { i32, i32 }* %_1 to i32*
    store i32 0, i32* %0, align 4
    %1 = getelementptr inbounds { i32, i32 }, { i32, i32 }* %_1, i32 0, i32 0
    %2 = load i32, i32* %1, align 4, !range !0
    %3 = getelementptr inbounds { i32, i32 }, { i32, i32 }* %_1, i32 0, i32 1
    %4 = load i32, i32* %3, align 4
  ; call monomorphization::generic_func
    %5 = call { i32, i32 } @_ZN16monomorphization12generic_func17hdce2324413889031E(i32 %2, i32 %4)
    %6 = extractvalue { i32, i32 } %5, 0
    %7 = extractvalue { i32, i32 } %5, 1
    br label %bb1

  bb1:                                              ; preds = %start
    %8 = insertvalue { i32, i32 } undef, i32 %6, 0
    %9 = insertvalue { i32, i32 } %8, i32 %7, 1
    ret { i32, i32 } %9
  }

  ...

見ての通り， ``i32`` と ``Option<i32>`` 用にそれぞれ ``generic_func`` のコードが生成されていて， ``test_func1`` と ``test_func2`` は ``i32`` 用のを， ``test_func3`` は ``Option<i32>`` 用のを呼んでることが分かる．

メタデータの内容
----------------

メタデータは， Rust コンパイラの `rustc_metadata crate <https://github.com/rust-lang/rust/tree/4edff843dd219cf19a5fede6c78c7ce95402e1f5/src/librustc_metadata>`_ で取り扱っているっぽい．まだ，よく分かってない部分もあるんだけど，メタデータは大体以下のフォーマットになってるっぽい:

1. 最初の 7 byte はヘッダで， ``[0, 0, 0, 0, b'r', b'u', b's', b't', 0, 0, 0]`` (``0000 0000 7275 7374 0000 00``) 固定．
2. 次の 1 byte はフォーマットバージョンで，現状はバージョン 4 (``04``)
3. 次の 4 byte は，書き込んだバイト数が入っている．
4. 次はビルドに使った ``rustc`` のバージョンが， ``String`` をシリアライズした形で埋め込まれている．つまり，最初に文字列の長さ，次にその本体が続く感じ．ただし， ``usize`` 型の文字列の長さは， LEB128 エンコードされていて，短い文字列なら 1 byte で表現される．今回の環境では，

   ::

     2b72 7573 7463 2031 2e33 372e 302d 6e69  +rustc 1.37.0-ni
     6768 746c 7920 2834 6564 6666 3834 3364  ghtly (4edff843d
     2032 3031 392d 3036 2d31 3629            2019-06-16)

   となっていた． ``0x2b = 43`` なので合ってそう．

5. 次に `CrateRoot <https://github.com/rust-lang/rust/blob/4edff843dd219cf19a5fede6c78c7ce95402e1f5/src/librustc_metadata/schema.rs#L156>`_ をシリアライズしたものが，埋め込まれる．

メタデータのエンコードは次の関数を用いるっぽい:

.. code-block:: rust

  // https://github.com/rust-lang/rust/blob/4edff843dd219cf19a5fede6c78c7ce95402e1f5/src/librustc_metadata/encoder.rs#L1866
  pub fn encode_metadata<'tcx>(tcx: TyCtxt<'tcx>) -> EncodedMetadata { ... }

``TyCtxt`` は typing context と呼ばれてるけど，ぶっちゃけ色々入ってるやつ．多分 ``crate`` ごとに一つ用意されていて， ``crate`` の情報と型情報などが入っている． ``EncodedMetadata`` はメタデータがバイト列にエンコードされたものを表す型だけど，実体はただの ``Vec<u8>`` の newtype になっている:

.. code-block:: rust

  // https://github.com/rust-lang/rust/blob/4edff843dd219cf19a5fede6c78c7ce95402e1f5/src/librustc/middle/cstore.rs#L150
  pub struct EncodedMetadata {
      pub raw_data: Vec<u8>
  }

この関数の主要部分は ``ecx.encode_crate_root();`` で，ここで ``CrateRoot`` のシリアライズを行なっている．デコードは，次の部分でやるっぽい:

.. code-block:: rust

  // https://github.com/rust-lang/rust/blob/4edff843dd219cf19a5fede6c78c7ce95402e1f5/src/librustc_metadata/decoder.rs#L369
  impl<'tcx> MetadataBlob {
      pub fn is_compatible(&self) -> bool { ... }

      pub fn get_rustc_version(&self) -> String { ... }

      pub fn get_root(&self) -> CrateRoot<'tcx> { ... }

      pub fn list_crate_metadata(&self,
                                out: &mut dyn io::Write) -> io::Result<()>
                                { ... }
  }

``MetadataBlob`` はメタデータのバイトフォーマットデータを表すデータ型で，やはり実体は ``u8`` 列のスライスになっている．ただ，こっちは高速化のため色々やってるっぽくて，その為の保証情報も付随してる．それぞれのメソッドは，

* ``is_compatible``: メタデータのヘッダとバージョンチェック
* ``get_rustc_version``: ``rustc`` のバージョンをデシリアライズして取得
* ``get_root``: ``CrateRoot`` をデシリアライズして取得
* ``list_crate_metadata``: ``get_root`` で取ってきた ``CrateRoot`` から，外部ファイルで依存している名前を一覧表示 [#list-crate-metadata-usecase]_ ．

という感じっぽい．で，肝心のどういう情報が載ってるかだけど，それは ``CrateRoot`` の定義を見れば良くて，フィールドの名前から推察する限り，以下の情報が入ってそう．

* ``crate`` の情報 (名前 / ターゲット / エディションなど)
* 外部やネイティブライブラリへの依存関係
* trait の実装やエクスポートしているシンボル情報
* ``entries_index``

で， ``entries_index`` が今回重要な情報で，こいつには `Entry <https://github.com/rust-lang/rust/blob/4edff843dd219cf19a5fede6c78c7ce95402e1f5/src/librustc_metadata/schema.rs#L211>`_ 要素が格納されている位置がたくさん詰め込まれている． ``Entry`` は以下の種類のデータを持っているっぽい:

.. code-block:: rust

  // https://github.com/rust-lang/rust/blob/4edff843dd219cf19a5fede6c78c7ce95402e1f5/src/librustc_metadata/schema.rs#L231
  pub enum EntryKind<'tcx> {
      Const(ConstQualif, Lazy<RenderedConst>),
      ImmStatic,
      MutStatic,
      ForeignImmStatic,
      ForeignMutStatic,
      ForeignMod,
      ForeignType,
      GlobalAsm,
      Type,
      TypeParam,
      ConstParam,
      Existential,
      Enum(ReprOptions),
      Field,
      Variant(Lazy<VariantData<'tcx>>),
      Struct(Lazy<VariantData<'tcx>>, ReprOptions),
      Union(Lazy<VariantData<'tcx>>, ReprOptions),
      Fn(Lazy<FnData<'tcx>>),
      ForeignFn(Lazy<FnData<'tcx>>),
      Mod(Lazy<ModData>),
      MacroDef(Lazy<MacroDef>),
      Closure(Lazy<ClosureData<'tcx>>),
      Generator(Lazy<GeneratorData<'tcx>>),
      Trait(Lazy<TraitData<'tcx>>),
      Impl(Lazy<ImplData<'tcx>>),
      Method(Lazy<MethodData<'tcx>>),
      AssocType(AssocContainer),
      AssocExistential(AssocContainer),
      AssocConst(AssocContainer, ConstQualif, Lazy<RenderedConst>),
      TraitAlias(Lazy<TraitAliasData<'tcx>>),
  }

で，全種類において，可視性 / 属性 / 安定性情報や，型情報 / ジェネリクス / 境界条件の情報，本体の MIR コードなどが入っているっぽい．

メタデータの MIR を確認する
---------------------------

``rustc`` の nightly 版には， ``rustc_private`` という feature が用意されていて，それを使うと ``rustc`` 内の crate を使える．それを使えば何とかなると思ったんだけど，なんか色々辛くて，コンパイラに直接コードを埋め込むことにした．

``rustc`` のコンパイラ本体コードは， `rustc_driver crate <https://github.com/rust-lang/rust/tree/4edff843dd219cf19a5fede6c78c7ce95402e1f5/src/librustc_driver>`_ で扱っているっぽい．ここの `run_compiler <https://github.com/rust-lang/rust/blob/4edff843dd219cf19a5fede6c78c7ce95402e1f5/src/librustc_driver/lib.rs#L124>`_ に次のコードを追加する:

.. code-block:: rust

  ...

  use rustc::hir::def_id::DefIndex;
  use rustc::hir::def::DefKind;
  use rustc_mir::util::pretty::write_basic_block;

  ...

  if sess.opts.debugging_opts.save_analysis {
      mem::drop(compiler.expansion()?.take());
  }

  // inspect crate metadata code
  //----------------------------------------
  compiler.cstore().iter_crate_data(|_, cmeta| {
      let crate_name_str = String::from(cmeta.name.as_str().get());
      let target_crate_name_prefix = "monomorphization_";
      if crate_name_str.starts_with(target_crate_name_prefix) {
          let idx_num: u32 = crate_name_str[target_crate_name_prefix.len()..]
              .parse().unwrap()
              ;
          let def_index = DefIndex::from(idx_num);

          let entry_name = cmeta.item_name(def_index).as_str();

          if let Some(DefKind::Fn) = cmeta.def_kind(def_index) {
              compiler.global_ctxt().unwrap().peek_mut().enter(|tcx| {
                  println!("{}: {}", entry_name, cmeta.fn_sig(def_index, tcx));

                  if let Some(body) = cmeta.maybe_get_optimized_mir(tcx, def_index) {
                      for block in body.basic_blocks().indices() {
                          write_basic_block(tcx, block, &body, &mut |_, _| Ok(()), &mut io::stdout()).unwrap();
                          if block.index() + 1 != body.basic_blocks().len() {
                              println!("");
                          }
                      }
                  } else {
                      println!("Cannot found MIR body.");
                  }

              });
          } else {
              println!("{}: This entry is not function.", entry_name);
          }
      }
  });
  //----------------------------------------

  compiler.ongoing_codegen()?;

``compiler.cstore()`` は ``CrateStore`` データが入っており，こいつに crate のメタデータが詰め込まれている．こいつから

1. 中身のクレートをそれぞれ見て，名前が ``monomorphization_[0-9]*`` となっているものを引っ張ってくる．
2. 後ろに付いている番号を ``Entry`` のインデックスとして，メタデータからそのエントリの情報をもらってくる．
3. エントリの中身を表示する．

ということをする．ただ， ``Entry`` 自体はパブリックに使えないデータになっていて， ``EntryKind`` も同様なので直接は扱えない．で， ``EntryKind`` に相当するパブリックなデータ型があり，それが ``DefKind`` ．一回こいつでエントリの種別を取得し，関数ならその関数の型シグネチャと MIR コードを表示する．ただこいつらの取得には ``TyCtxt`` が必要になる．こいつはスレッドローカルストレージに格納されていて，取り出すときは色々面倒な操作が必要になる．それを取り出してるのが，  ``compiler.global_ctxt().unwrap().peek_mut().enter(|tcx| { ... })`` の部分．後， MIR は制御フローグラフのノード (ブロック) の塊になっているので，そいつをいい感じに表示するためのコードも入れてる．ただ，このコードはこれだけでは動かなくって， `CrateStore.iter_crate_data <https://github.com/rust-lang/rust/blob/4edff843dd219cf19a5fede6c78c7ce95402e1f5/src/librustc_metadata/cstore.rs#L135>`_ と `write_basic_block <https://github.com/rust-lang/rust/blob/4edff843dd219cf19a5fede6c78c7ce95402e1f5/src/librustc_mir/util/pretty.rs#L302>`_ が必要になるためこいつをパブリックにした:

.. code-block:: diff

  // https://github.com/rust-lang/rust/blob/4edff843dd219cf19a5fede6c78c7ce95402e1f5/src/librustc_metadata/cstore.rs#L135
  - pub(super) fn iter_crate_data<I>(&self, mut i: I)
  + pub fn iter_crate_data<I>(&self, mut i: I)

  // https://github.com/rust-lang/rust/blob/4edff843dd219cf19a5fede6c78c7ce95402e1f5/src/librustc_mir/util/mod.rs#L13
  - pub(super) mod pretty
  + pub mod pretty

で，このコードを埋め込んで ``rustc`` をコンパイルする． https://rust-lang.github.io/rustc-guide/how-to-build-and-run.html の通りに，

::

  cp config.toml.example config.toml
  sed -ie 's/#debug = false/debug = true/' config.toml
  ./x.py build

みたいなことをすればいい．なお，実際に使った ``config.toml`` は以下の感じ:

.. code-block:: diff

  --- config.toml.example
  +++ config.toml
  @@ -27,7 +27,7 @@
   #release-debuginfo = false

   # Indicates whether the LLVM assertions are enabled or not
  -#assertions = false
  +assertions = true

   # Indicates whether ccache is used when building LLVM
   #ccache = false
  @@ -136,7 +136,7 @@
   # Flag to specify whether any documentation is built. If false, rustdoc and
   # friends will still be compiled but they will not be used to generate any
   # documentation.
  -#docs = true
  +docs = false

   # Indicate whether the compiler should be documented in addition to the standard
   # library and facade crates.
  @@ -263,7 +263,7 @@
   # Note: the slowness of the non optimized compiler compiling itself usually
   #       outweighs the time gains in not doing optimizations, therefore a
   #       full bootstrap takes much more time with `optimize` set to false.
  -#optimize = true
  +optimize = true

   # Indicates that the build should be configured for debugging Rust. A
   # `debug`-enabled compiler and standard library will be somewhat
  @@ -286,7 +286,7 @@
   #       "maximally debuggable" environment (notably libstd) takes
   #       hours to build.
   #
  -#debug = false
  +debug = true

   # Number of codegen units to use for each compiler invocation. A value of 0
   # means "the number of cores on this machine", and 1+ is passed through to the
  @@ -324,10 +324,10 @@
   #debuginfo-level-tests = 0

   # Whether or not `panic!`s generate backtraces (RUST_BACKTRACE)
  -#backtrace = true
  +backtrace = true

   # Whether to always use incremental compilation when building rustc
  -#incremental = false
  +incremental = true

   # Build a multi-threaded rustc
   #parallel-compiler = false
  @@ -340,7 +340,7 @@
   # The "channel" for the Rust build to produce. The stable/beta channels only
   # allow using stable features, whereas the nightly and dev channels allow using
   # nightly features
  -#channel = "dev"
  +channel = "dev"

   # By default the `rustc` executable is built with `-Wl,-rpath` flags on Unix
   # platforms to ensure that the compiler is usable by default from the build
  @@ -353,12 +353,12 @@
   #verbose-tests = false

   # Flag indicating whether tests are compiled with optimizations (the -O flag).
  -#optimize-tests = true
  +optimize-tests = false

   # Flag indicating whether codegen tests will be run or not. If you get an error
   # saying that the FileCheck executable is missing, you may want to disable this.
   # Also see the target's llvm-filecheck option.
  -#codegen-tests = true
  +codegen-tests = false

   # Flag indicating whether git info will be retrieved from .git automatically.
   # Having the git information can cause a lot of rebuilds during development.
  @@ -408,7 +408,7 @@
   #deny-warnings = true

   # Print backtrace on internal compiler errors during bootstrap
  -#backtrace-on-ice = false
  +backtrace-on-ice = true

   # Whether to verify generated LLVM IR
   #verify-llvm-ir = false

なお， ``rustup`` でこのビルドしたコンパイラを，次のように登録できるらしい::

  rustup toolchain link local-build build/x86_64*/stage2

便利だ．後は使いたいディレクトリで ``rustup override set local-build`` とかすればいい．で，まず ``hir-tree`` からみたいエントリのインデックスを特定する::

  $ rustc -Zunpretty=hir-tree monomorphization.rs
  ...
          HirId {
              owner: DefIndex(12),
              local_id: 0,
          }: Item {
              ident: generic_func#0,
              hir_id: HirId {
                  owner: DefIndex(12),
                  local_id: 0,
              },
              attrs: [],
              node: Fn(
                  FnDecl {
                      inputs: [
                          type(T),
                      ],
                      output: Return(
                          type(T),
                      ),
                      c_variadic: false,
                      implicit_self: None,
                  },
  ...

``generic_func`` は 12 番っぽい．この番号を元に次のようにして，メタデータを見る::

  $ rustc --crate-type=lib --crate-name=monomorphization_12 monomorphization.rs
  $ cat > main.rs
  extern crate monomorphization_12;

  fn main() {
    println!("{:?}", monomorphization_12::generic_func(0));
  }
  $ rustc -L . main.rs
  generic_func: fn(T) -> T
    bb0: {
        _0 = move _1;                    // bb0[0]: scope 0 at monomorphization.rs:4:3: 4:4
        return;                          // bb0[1]: scope 0 at monomorphization.rs:5:2: 5:2
    }

このコードが，元の ``monomorphization.rs`` の ``generic_func`` 関数の MIR コードと一致することは，以下のように確認できる::

  $ rustc --crate-type=lib -Zunpretty=mir monomorphization.rs
  ...

  fn  generic_func(_1: T) -> T {
      let mut _0: T;                       // return place in scope 0 at monomorphization.rs:3:33: 3:34

      bb0: {
          _0 = move _1;                    // bb0[0]: scope 0 at monomorphization.rs:4:3: 4:4
          return;                          // bb0[1]: scope 0 at monomorphization.rs:5:2: 5:2
      }
  }

  ...

境界条件を伴ったジェネリック関数
--------------------------------

トレイトの境界条件が付いた場合は，どのようなコードが生成されるかも見てみる．まず，次のコードを用意する:

.. code-block:: rust

  // trait_monomorphization.rs

  pub trait TestTrait {
    fn test_trait_func(self) -> Self;
  }

  pub fn generic_trait_func<T: TestTrait>(x: T) -> T {
    x.test_trait_func()
  }

  impl TestTrait for i32 {
    fn test_trait_func(self) -> Self {
      self
    }
  }

  impl TestTrait for Option<i32> {
    fn test_trait_func(self) -> Self {
      self
    }
  }

  pub fn test_func1() -> i32 {
    generic_trait_func(0)
  }

  pub fn test_func2() -> i32 {
    generic_trait_func(333)
  }

  pub fn test_func3() -> Option<i32> {
    generic_trait_func(None)
  }

こいつの， MIR コードは以下のようになる::

  $ rustc --crate-type lib -Zunpretty=mir trait_monomorphization.rs
  // WARNING: This output format is intended for human consumers only
  // and is subject to change without notice. Knock yourself out.
  fn  <impl at trait_monomorphization.rs:17:1: 21:2>::test_trait_func(_1: std::option::Option<i32>) -> std::option::Option<i32> {
      let mut _0: std::option::Option<i32>; // return place in scope 0 at trait_monomorphization.rs:18:31: 18:35

      bb0: {
          _0 = _1;                         // bb0[0]: scope 0 at trait_monomorphization.rs:19:5: 19:9
          return;                          // bb0[1]: scope 0 at trait_monomorphization.rs:20:4: 20:4
      }
  }

  fn  generic_trait_func(_1: T) -> T {
      let mut _0: T;                       // return place in scope 0 at trait_monomorphization.rs:7:50: 7:51
      let mut _2: T;                       // in scope 0 at trait_monomorphization.rs:8:3: 8:4

      bb0: {
          StorageLive(_2);                 // bb0[0]: scope 0 at trait_monomorphization.rs:8:3: 8:4
          _2 = move _1;                    // bb0[1]: scope 0 at trait_monomorphization.rs:8:3: 8:4
          _0 = const <T as TestTrait>::test_trait_func(move _2) -> bb1; // bb0[2]: scope 0 at trait_monomorphization.rs:8:3: 8:22
                                          // ty::Const
                                          // + ty: fn(T) -> T {<T as TestTrait>::test_trait_func}
                                          // + val: Scalar(<ZST>)
                                          // mir::Constant
                                          // + span: trait_monomorphization.rs:8:5: 8:20
                                          // + ty: fn(T) -> T {<T as TestTrait>::test_trait_func}
                                          // + literal: Const { ty: fn(T) -> T {<T as TestTrait>::test_trait_func}, val: Scalar(<ZST>) }
      }

      bb1: {
          StorageDead(_2);                 // bb1[0]: scope 0 at trait_monomorphization.rs:8:21: 8:22
          return;                          // bb1[1]: scope 0 at trait_monomorphization.rs:9:2: 9:2
      }
  }

  fn  <impl at trait_monomorphization.rs:11:1: 15:2>::test_trait_func(_1: i32) -> i32 {
      let mut _0: i32;                     // return place in scope 0 at trait_monomorphization.rs:12:31: 12:35

      bb0: {
          _0 = _1;                         // bb0[0]: scope 0 at trait_monomorphization.rs:13:5: 13:9
          return;                          // bb0[1]: scope 0 at trait_monomorphization.rs:14:4: 14:4
      }
  }

  fn  test_func1() -> i32 {
      let mut _0: i32;                     // return place in scope 0 at trait_monomorphization.rs:23:24: 23:27

      bb0: {
          _0 = const generic_trait_func::<i32>(const 0i32) -> bb1; // bb0[0]: scope 0 at trait_monomorphization.rs:24:3: 24:24
                                          // ty::Const
                                          // + ty: fn(i32) -> i32 {generic_trait_func::<i32>}
                                          // + val: Scalar(<ZST>)
                                          // mir::Constant
                                          // + span: trait_monomorphization.rs:24:3: 24:21
                                          // + ty: fn(i32) -> i32 {generic_trait_func::<i32>}
                                          // + literal: Const { ty: fn(i32) -> i32 {generic_trait_func::<i32>}, val: Scalar(<ZST>) }
                                          // ty::Const
                                          // + ty: i32
                                          // + val: Scalar(0x00000000)
                                          // mir::Constant
                                          // + span: trait_monomorphization.rs:24:22: 24:23
                                          // + ty: i32
                                          // + literal: Const { ty: i32, val: Scalar(0x00000000) }
      }

      bb1: {
          return;                          // bb1[0]: scope 0 at trait_monomorphization.rs:25:2: 25:2
      }
  }

  fn  test_func3() -> std::option::Option<i32> {
      let mut _0: std::option::Option<i32>; // return place in scope 0 at trait_monomorphization.rs:31:24: 31:35
      let mut _1: std::option::Option<i32>; // in scope 0 at trait_monomorphization.rs:32:22: 32:26

      bb0: {
          StorageLive(_1);                 // bb0[0]: scope 0 at trait_monomorphization.rs:32:22: 32:26
          discriminant(_1) = 0;            // bb0[1]: scope 0 at trait_monomorphization.rs:32:22: 32:26
          _0 = const generic_trait_func::<std::option::Option<i32>>(move _1) -> bb1; // bb0[2]: scope 0 at trait_monomorphization.rs:32:3: 32:27
                                          // ty::Const
                                          // + ty: fn(std::option::Option<i32>) -> std::option::Option<i32> {generic_trait_func::<std::option::Option<i32>>}
                                          // + val: Scalar(<ZST>)
                                          // mir::Constant
                                          // + span: trait_monomorphization.rs:32:3: 32:21
                                          // + ty: fn(std::option::Option<i32>) -> std::option::Option<i32> {generic_trait_func::<std::option::Option<i32>>}
                                          // + literal: Const { ty: fn(std::option::Option<i32>) -> std::option::Option<i32> {generic_trait_func::<std::option::Option<i32>>}, val: Scalar(<ZST>) }
      }

      bb1: {
          StorageDead(_1);                 // bb1[0]: scope 0 at trait_monomorphization.rs:32:26: 32:27
          return;                          // bb1[1]: scope 0 at trait_monomorphization.rs:33:2: 33:2
      }
  }

  fn  test_func2() -> i32 {
      let mut _0: i32;                     // return place in scope 0 at trait_monomorphization.rs:27:24: 27:27

      bb0: {
          _0 = const generic_trait_func::<i32>(const 333i32) -> bb1; // bb0[0]: scope 0 at trait_monomorphization.rs:28:3: 28:26
                                          // ty::Const
                                          // + ty: fn(i32) -> i32 {generic_trait_func::<i32>}
                                          // + val: Scalar(<ZST>)
                                          // mir::Constant
                                          // + span: trait_monomorphization.rs:28:3: 28:21
                                          // + ty: fn(i32) -> i32 {generic_trait_func::<i32>}
                                          // + literal: Const { ty: fn(i32) -> i32 {generic_trait_func::<i32>}, val: Scalar(<ZST>) }
                                          // ty::Const
                                          // + ty: i32
                                          // + val: Scalar(0x0000014d)
                                          // mir::Constant
                                          // + span: trait_monomorphization.rs:28:22: 28:25
                                          // + ty: i32
                                          // + literal: Const { ty: i32, val: Scalar(0x0000014d) }
      }

      bb1: {
          return;                          // bb1[0]: scope 0 at trait_monomorphization.rs:29:2: 29:2
      }
  }

トレイトのメソッド呼び出しが， ``<T as Trait>::method`` みたいな形に変わり，境界条件はなくなっている． LLVM IR も見てみる::

  $ rustc --crate-type=lib --emit=llvm-ir trait_monomorphization.rs
  $ cat trait_monomorphization.ll
  ...

  ; trait_monomorphization::generic_trait_func
  ; Function Attrs: uwtable
  define { i32, i32 } @_ZN22trait_monomorphization18generic_trait_func17h2353f770a11f3463E(i32 %x.0, i32 %x.1) unnamed_addr #0 {
  start:
  ; call <core::option::Option<i32> as trait_monomorphization::TestTrait>::test_trait_func
    %0 = call { i32, i32 } @"_ZN85_$LT$core..option..Option$LT$i32$GT$$u20$as$u20$trait_monomorphization..TestTrait$GT$15test_trait_func17h91469f85fa74f82cE"(i32 %x.0, i32 %x.1)
    %1 = extractvalue { i32, i32 } %0, 0
    %2 = extractvalue { i32, i32 } %0, 1
    br label %bb1

  bb1:                                              ; preds = %start
    %3 = insertvalue { i32, i32 } undef, i32 %1, 0
    %4 = insertvalue { i32, i32 } %3, i32 %2, 1
    ret { i32, i32 } %4
  }

  ; trait_monomorphization::generic_trait_func
  ; Function Attrs: uwtable
  define i32 @_ZN22trait_monomorphization18generic_trait_func17h888a1bef68f0d18bE(i32 %x) unnamed_addr #0 {
  start:
  ; call <i32 as trait_monomorphization::TestTrait>::test_trait_func
    %0 = call i32 @"_ZN57_$LT$i32$u20$as$u20$trait_monomorphization..TestTrait$GT$15test_trait_func17hc700a3de885bafa4E"(i32 %x)
    br label %bb1

  bb1:                                              ; preds = %start
    ret i32 %0
  }

  ; <i32 as trait_monomorphization::TestTrait>::test_trait_func
  ; Function Attrs: uwtable
  define i32 @"_ZN57_$LT$i32$u20$as$u20$trait_monomorphization..TestTrait$GT$15test_trait_func17hc700a3de885bafa4E"(i32 %self) unnamed_addr #0 {
  start:
    ret i32 %self
  }

  ; <core::option::Option<i32> as trait_monomorphization::TestTrait>::test_trait_func
  ; Function Attrs: uwtable
  define { i32, i32 } @"_ZN85_$LT$core..option..Option$LT$i32$GT$$u20$as$u20$trait_monomorphization..TestTrait$GT$15test_trait_func17h91469f85fa74f82cE"(i32 %self.0, i32 %self.1) unnamed_addr #0 {
  start:
    %0 = insertvalue { i32, i32 } undef, i32 %self.0, 0
    %1 = insertvalue { i32, i32 } %0, i32 %self.1, 1
    ret { i32, i32 } %1
  }

  ; trait_monomorphization::test_func1
  ; Function Attrs: uwtable
  define i32 @_ZN22trait_monomorphization10test_func117h34f6be1b1f5ee768E() unnamed_addr #0 {
  start:
  ; call trait_monomorphization::generic_trait_func
    %0 = call i32 @_ZN22trait_monomorphization18generic_trait_func17h888a1bef68f0d18bE(i32 0)
    br label %bb1

  bb1:                                              ; preds = %start
    ret i32 %0
  }

  ; trait_monomorphization::test_func2
  ; Function Attrs: uwtable
  define i32 @_ZN22trait_monomorphization10test_func217h4d629592fd7a0d2cE() unnamed_addr #0 {
  start:
  ; call trait_monomorphization::generic_trait_func
    %0 = call i32 @_ZN22trait_monomorphization18generic_trait_func17h888a1bef68f0d18bE(i32 333)
    br label %bb1

  bb1:                                              ; preds = %start
    ret i32 %0
  }

  ; trait_monomorphization::test_func3
  ; Function Attrs: uwtable
  define { i32, i32 } @_ZN22trait_monomorphization10test_func317ha2174098ce8095a3E() unnamed_addr #0 {
  start:
    %_1 = alloca { i32, i32 }, align 4
    %0 = bitcast { i32, i32 }* %_1 to i32*
    store i32 0, i32* %0, align 4
    %1 = getelementptr inbounds { i32, i32 }, { i32, i32 }* %_1, i32 0, i32 0
    %2 = load i32, i32* %1, align 4, !range !0
    %3 = getelementptr inbounds { i32, i32 }, { i32, i32 }* %_1, i32 0, i32 1
    %4 = load i32, i32* %3, align 4
  ; call trait_monomorphization::generic_trait_func
    %5 = call { i32, i32 } @_ZN22trait_monomorphization18generic_trait_func17h2353f770a11f3463E(i32 %2, i32 %4)
    %6 = extractvalue { i32, i32 } %5, 0
    %7 = extractvalue { i32, i32 } %5, 1
    br label %bb1

  bb1:                                              ; preds = %start
    %8 = insertvalue { i32, i32 } undef, i32 %6, 0
    %9 = insertvalue { i32, i32 } %8, i32 %7, 1
    ret { i32, i32 } %9
  }

  ...

やはり，普通のジェネリック関数と同じで，型ごとに特殊化されたコードが生成され，メソッドの実装もそれぞれ埋め込まれるっぽい．なお，表示が同じになるのでやらないけど，メタデータに， ``generic_func`` の時と同じように ``generic_trait_func`` の MIR コードも埋め込まれる．

まとめ
------

というわけで， Rust のジェネリック関数は基本，関数が使われる場所を解析して，その場所の型ごとに特殊化したコードを生成しそれを使うようにする．ただ，パブリックなジェネリック関数はそのコード情報を crate に埋め込む必要があるので，メタデータに MIR コードとして入れておく．これはトレイトによる境界条件があっても変わらない，という感じっぽい．基本 C++ のテンプレートと同じで，ソース中にそのまま書ける感じかな．

``rustc`` の解読，辛かったけど，色々情報を手に入れた．結構 ``rustc`` で情報を見れるの良さそう．後， ``rustup`` をそのままビルドしたコードに使えるのもいいっすね．

.. [#mir-based-borrow-check] 元々は HIR で借用検査 (borrow check) を行なっていたっぽいが， Rust 2018 で入った NLL (non-lexical lifetime) の為に MIR でやるようにしたっぽい．
.. [#list-crate-metadata-usecase] 正直，何に使うかはよく分かってない．
