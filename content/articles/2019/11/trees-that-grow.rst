type family を使って再帰的 ADT をオープンにする
===============================================

:date: 2019-11-17 18:00
:tags: Haskell, GHC, GHC拡張
:category: プログラミング

元ネタは `Trees that grow <https://www.microsoft.com/en-us/research/publication/trees-that-grow/>`_ ． Haskell では代数的データ型 (ADT) を使ってプログミングに使うデータ構造を定義し，その構造を操作することによりプログラミングを行う． ADT はパターンマッチが容易で，再帰的に定義でき，基本的に閉じた構造になっている．そのため便利な反面，その機能が保守で仇となる場合もある．この問題は古くから知られており，いくつかの解決策も提案されてきた．今回はこのうち，現在 GHC で採用されつつある type family を使った解決方法を紹介する．

なお，環境として以下を想定している．

+--------------------+---------+
| GHC のバージョン   | 8.8.1   |
+--------------------+---------+

The Expression Problem
----------------------

プログラミング，特に Haskell を使用したプログラミングにおいて，データ型は非常に重要な役割を持つ．特に，一部のプログラムにおいては，根幹をなすデータ型がいくつか存在するような場合もある．この場合データ型の扱いをどうするかは設計段階において非常に重要になる．例えば，コンパイラを作る場合を想定すると，多くの場合 AST の定義とコンテキストの定義が非常に重要になるだろう．この場合に， AST に話を絞ると，プログラムのフェーズによって AST に対し少し情報を足したりしたい場合がある．この場合データ型自体は異なるが，ほぼ基盤は変わらない．このため，それぞれデータ型を定義するとなると，共通の処理関数やインターフェースなども再定義する必要に迫られ，ボイラープレートが大量に生まれることになる．また，元々あった構文を脱糖し，その後はその構文がない前提で話を進めたい場合などもあるだろう．この場合も，脱糖後用に新たにデータ型を定義し直すのは手間である．

一般に上のように，場合分け可能なデータ型に対し，既存の処理関数やデータ型を再定義することなく，新たな場合分けを追加できるかという問題は Expression Problem と名付けられている [#the-expression-problem-detail]_ ．特に純粋な ADT しか持たないような Haskell などの言語は Expression Problem に弱く， Scala などのオブジェクト指向を採用してる言語はこのような問題にはかなり強い．例えば，足算を行う電卓の例を考えてみる． AST の定義とそれに対しての実行関数だけ書いてみると，それぞれ以下のようになるだろう:

.. code-block:: haskell

  -- Haskell

  data Ast
    = Add Ast Ast
    | Num Int

  eval :: AST -> Int
  eval (Add x y) = eval x + eval y
  eval (Num i)   = i

.. code-block:: scala

  // Scala

  trait Ast

  case class Add(x: Ast, y: Ast) extends Ast
  case class Num(i: Int) extends Ast

  def eval(t: Ast): Int = t match {
    case Add(x, y) => eval(x) + eval(y)
    case Num(i)    => i
  }

これらのプログラムで遊び終えたら，多くの人はこの電卓に新たな機能を足したいと思うはずだ．例えば，掛け算を追加してみることを考えたい．上のプログラムを全く変えないで，行末に何かを追加するだけで，掛け算も計算できるようにならないだろうか？ 残念ながら今回の場合は，不可能だ．しかし， Scala の場合少し工夫するだけでこの目標を達成できる．最初のプログラムを以下のように作っておくのだ:

.. code-block:: scala

  // Scala

  trait Ast {
    def eval(): Int
  }

  case class Add(x: Ast, y: Ast) extends Ast {
    def eval(): Int = x.eval() + y.eval()
  }

  case class Num(i: Int) extends Ast {
    def eval(): Int = i
  }

この場合，掛け算を追加したかったら，行末に以下を追記すれば良い:

.. code-block:: scala

  case class Mul(x: Ast, y: Ast) extends Ast {
    def eval(): Int = x.eval() * y.eval()
  }

残念ながら， Haskell では同じことをやろうとすると，大規模な改築が必要になる．原因はデータ型が再帰的であることによる． ``Ast`` は再帰的であるため，単純に以下のようなことをしても掛け算を追加したことにはならない:

.. code-block:: haskell

  data Ast2
    = Orig Ast
    | Mul Ast2 Ast2

なぜなら， ``(2 * 3) + 1`` のようなものをこのデータ型では表現できないからだ．つまり，元々の再帰構造自体はデータ型を定義した時点で既に確定してしまっており，後から付け入る隙がないのだ．逆に Scala では再帰部分をベースのトレイトで定義しており，後から派生クラスをいくらでも追加することができる．一般にオブジェクト指向型の言語では，再帰部分をオブジェクトの基底インターフェースにすることで，自然なプログラミングスタイルながら後から再帰部分をいくらでも派生させることができる．このような特性は，開いた再帰 (open recursion) と呼ばれる．これは，オブジェクト指向を搭載する言語の大きな強みだと個人的には思っている [#open-vs-close]_ ．

Haskell では，この問題の解決が結構昔から取り組まれており， tagless final [#finally-tagless-ref]_ ， data types a la carte [#data-types-a-la-carte-ref]_ などの手法が存在する．今回は，これらの提案の手法のうち，比較的新しく GHC で使われている， type family を使った手法について紹介する．

Trees That Grow
---------------

この手法の面白いところは，他の手法と比べ，かなり自然な Haskell プログラミングの形でデータ型を拡張できることにある．つまりかなり単純な手法で，拡張性を持つデータ型を扱え，オープン性を type family のオープン性を使って担保するだけだ．

type family とは， GHC の言語拡張で提供される機能で，その名の通り型の族，つまりある型に対して別の型を結びつけるような写像を定義できる機能だ．例えば，

.. code-block:: haskell

  {-# LANGUAGE TypeFamilies #-}
  {-# LANGUAGE PolyKinds    #-}

  import Data.Kind

  type family ElemType (c :: Type) :: Type

  type instance ElemType [a]        = a
  type instance ElemType (Maybe a)  = a
  type instance ElemType Text       = Char
  type instance ElemType ByteString = Word8

のように書くと， ``ElemType String`` や ``ElemType Text`` が ``Char`` 型のエイリアスとして使えるようになる．型でパターンマッチできる，型エイリアスだと思っても良いだろう．ただ，この機能の面白いところは，パターンマッチを後からいくらでも足せるところにある [#closed-type-families]_　．この機能を使うと，上の電卓の例を次のように修正することができる．

.. code-block:: haskell

  {-# LANGUAGE ScopedTypeVariables #-}
  {-# LANGUAGE TypeApplications    #-}

  data Ast p
    = Add (Ast p) (Ast p)
    | Num Integer
    | XAst (XAst p)

  type family XAst (p :: Type) :: Type

  class EvalXAst p where
    evalXAst :: (Ast p -> Integer) -> XAst p -> Integer

  eval :: forall p. EvalXAst p => Ast p -> Integer
  eval = go
    where
      goXAst = evalXAst @p go

      go (Add x y) = go x + go y
      go (Num i)   = i
      go (XAst x)  = goXAst x


  data OldAst
  type instance XAst OldAst = Void

  instance EvalXAst OldAst where
    evalXAst _ x = absurd x


  data WithMul p = Mul (Ast p) (Ast p)

  data NewAst
  type instance XAst NewAst = WithMul NewAst

  instance EvalXAst NewAst where
    evalXAst go (Mul x y) = go x * go y

ちょっと複雑に見えるのが，基幹部は ``Ast p`` の ``XAst`` というデータコンストラクタだ． ``XAst`` データコンストラクタは type family で定義されたデータ型を受け取るようになっており， type family のインスタンスを後から挿入できるようになっている． ``EvalXAst`` は後から挿入するコンストラクタの，パターンマッチ部分を受けとるようになっていて， ``eval`` はそいつを受け取って完成するようになっている．その下が実際のパターンマッチを後付けしてる部分で， ``Ast OldAst`` は元々の足し算しかない電卓の動作， ``Ast NewAst`` は掛け算も追加した電卓の動作が行えるようになっている．このように， type family をデータ型に埋め込むことで，データ型を一部オープンにすることができるようになる．さらに，この手法は上の Scala の例と異なり，様々なバリエーションのデータ型を双方共存させることができる．

この手法は， Expression Problem 以外にも応用できる．最初にあげた，フェーズごとに異なる情報を入れるようなデータ型にも対応できる．例えば，

.. code-block:: haskell

  data Type
    = NumType
    | ArrowType Type Type

  data Ast p
    = App (Ast p) (Ast p)
    | Abs String (Ast p)
    | Var (XVar p) String
    | Num Integer

  type family XVar p

  data Parsed
  type instance XVar Parsed = NoExt

  data Renamed
  type instance XVar Renamed = String

  data TypeChecked
  type instance XVar TypeChecked = (XVar Renamed, Type)

みたいなデータ型を作ると，パース時は何の情報もないのが，リネーム時に元々の変数名を，型検査時に変数の型を， AST のデータ型に付与することができる．

さらに， type family のインスタンスはモジュールを超えて定義できるため，インポートするモジュールを変更することでデータ型に付加する拡張を変更することもできる．かなり応用が効くだろう．

GHC での利用
------------

GHC での移行計画は， `GHC Wiki <https://gitlab.haskell.org/ghc/ghc/wikis/implementing-trees-that-grow>`_ に記載されている．ゴールとして，

* GHC で使っている AST
* Template Haskell で使っている AST
* ``haskell-src-exts`` で使っている AST

を共通化するという壮大な計画のようだ．現在は， trees that grow 用の type family は， `HsExtension <https://downloads.haskell.org/~ghc/latest/docs/html/libraries/ghc-8.8.1/HsExtension.html>`_ モジュールに纏まっている．そして，内部の AST に関するデータ型は，次のようになっている:

.. code-block:: haskell

  data HsExpr p
    = HsVar (XVar p) (Located (IdP p))
    | ...
    | XExpr (XXExpr p)

  type instance XVar (GhcPass _) = NoExt
  ...
  type instance XXExpr (GhcPass _) = NoExt

それぞれの基幹となるデータ型は，次のようになっている:

.. code-block:: haskell

  {-# LANGUAGE DataKinds #-}

  data NoExt = NoExt
  data GhcPass (c :: Pass)
  data Pass = Parsed | Renamed | Typechecked

  type GhcPs = GhcPass 'Parsed
  type GhcRn = GhcPass 'Renamed
  type GhcTc = GhcPass 'Typechecked

例えば，演算子適用を表すコンストラクタは，次のようになっている:

.. code-block:: haskell

  type LHsExpr p = Located (HsExpr p)

  data HsExpr p
    = ...
    | OpApp (XOpApp p) (LHsExpr p) (LHsExpr p) (LHsExpr p)
    | ...

  type instance XOpApp GhcPs = NoExt
  type instance XOpApp GhcRn = Fixity
  type instance XOpApp GhcTc = Fixity

演算子適用は，パース時は全て優先順位同じで左結合として扱われ，リネーム時に結合や優先順位が解決される．その解決された情報が，リネーム時から入ってるというわけだ．で，拡張部分については随時制約が用意されていて，例えば ``HsExpr`` 用には，

.. code-block:: haskell

  type ForallXExpr (c :: * -> Constraint) (x :: *) =
       ( c (XVar            x)
       , ...
       , c (XXExpr          x)
       )

みたいなエイリアスが `HsExtension`_ モジュールにあったりする．クラスインスタンスを作りたいときは，このエイリアスを使って作っていくという感じになるだろう．その書き換えとともにパーサやリネーム部分のリファクタリング計画もあるようで，今現在遂行中という感じっぽい．とりあえず， GHC で ``X`` と付くデータ型が出てきたら， tree that grows のものと思っていいと思う．内部的にはそこまで本格的な対応は入っていなくて， ``XExpr`` コンストラクタなどは来ない前提で来たら ``panic`` にするという処理になっているっぽいけど．

まとめ
------

というわけで，今回は type family による拡張性を持ったデータ型の定義方法について紹介した．この手法は結構示唆に富んでいると思う． Haskell では通常その定義内で閉じたデータ型しか作れない訳だけど， type family を使うことで定義を外から容易に拡張できるようにできる訳だ．つまり，オープン性を type family により調整できる訳だ．最もオブジェクト指向では，多くの場合もっと細かく権限が制御できたりする訳だけど，残念ながら type family だとそこまで細かく制御はできない．細かい制御がそこまでいるかというのは議論の余地があるかもしれないけど，そこらへんも現在の機能でなんとかできるか考えてみると面白いかもなと思ったりした．

とりあえず， GHC で本格的に用いられるようになってきた機能なので， GHC のコードを読む時用と Haskell プログラミングの技術の一つとしてまとめておいた．実はそこまで真面目に元論文を読んでないので，機会があればもうちょっと真面目に元論文読んでおきたい．今回は以上．

.. [#the-expression-problem-detail] 名付け親は Wadler 先生で，大学のプログラミング言語開発チームの ML で初めて使ったとされている．ML に投稿されたメールは， http://homepages.inf.ed.ac.uk/wadler/papers/expression/expression.txt で公開されている．なお，厳密にはここでの定義は元々の定義と違っていて，元々の定義は「元々あるコードの再コンパイルやキャストなどの型変換操作を必要とせずに，新たな場合分けを追加できるか」というもの．
.. [#open-vs-close] デフォルトで open か close かは，それぞれ一長一短でもあり， open は拡張性がある反面，第三者が契約外の拡張を加えてしまう可能性があり，それらを保守の際きちんと管理する責任が生まれる．逆に close の場合そのような管理責任は生まれないが，拡張性のなさをボイラープレートなど冗長な作業により埋めなければならない．
.. [#finally-tagless-ref] http://okmij.org/ftp/tagless-final/index.html
.. [#data-types-a-la-carte-ref] https://dl.acm.org/citation.cfm?id=1394795
.. [#closed-type-families] なお，パターンマッチを後から足せないようにする機能も用意されており， ``type family`` 宣言の際 ``where`` を書くと，その後に書いたインスタンス以外はインスタンスを登録できないようになる．
