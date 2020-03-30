DerivingVia で deriving 戦略を模倣する
======================================

:tags: GHC, Haskell, GHC拡張
:category: プログラミング言語

Haskell では、ボイラープレートは非常に嫌われており、それを撲滅するため強力な型システムの他にも幾つか機能が組み込まれている。その中で非常に強力な機能が、deriving だ。この機能は、データ型の型クラスインスタンスを、その名の通り導出してくれる機能である。Haskell 標準では、幾つかの標準の型クラスにおいて、事前に用意されたアルゴリズムによる導出ができるだけの機能だったが、GHC ではさらに3つの戦略を追加し、広範囲の型クラスの導出が行えるようになっている。特に最近入った ``DerivingVia`` と呼ばれる拡張の戦略は、deriving の可能性を大いに引き上げてくれる拡張である。

ところで、実は ``DerivingVia`` があれば他の戦略は大方模倣できる。今回は、``DerivingVia`` で各戦略を模倣しながら、簡単な紹介をしていこうと思う。

型クラスインスタンスの導出
--------------------------

Haskell は、データ型を代数的データ型、つまり直積と直和とプリミティブ型から構築できる型で定義する言語である。このため、型クラスが直積と直和に閉じているようなものであれば、大半インスタンス実装は同じような流れになる。例えば、``Eq`` 型クラスの例を見てみる:

.. code-block:: haskell

  data D1
    = D1_C1 Int D1
    | D1_C2 Bool

  instance Eq D1 where
    D1_C1 x1 y1 == D1_C1 x1 y2 = x1 == x2 && y1 == y2
    D1_C1{}     == D1_C2{}     = False
    D1_C2{}     == D1_C1{}     = False
    D1_C2 b1    == D1_C2 b2    = b1 == b2

  data D2
    = D2_C1 Int D2
    | D2_C2 Bool

  instance Eq D2 where
    D2_C1 x1 y1 == D2_C1 x1 y2 = x1 == x2 && y1 == y2
    D2_C1{}     == D2_C2{}     = False
    D2_C2{}     == D2_C1{}     = False
    D2_C2 b1    == D2_C2 b2    = b1 == b2

このプログラムにおいて、``D1`` と ``D2`` は同型であり、実装もコンストラクタの名前が異なるだけでやってる内容は特に変わらない。さらには、このプログラムは直和に対するインスタンス実装と直積に対するインスタンス実装の合わせ技であり、

1. まず直和の等価性により異なるコンストラクタ同士の比較は偽に、同じコンストラクタの比較は中身の等価性で判断する
2. 中身は有限直積になっているため、有限直積の等価性により、それぞれの要素をポイントワイズに比較し、その結果の論理積をとる

というようになる。このようなインスタンス実装をいちいちデータ型毎に書くのは、Haskeller にとって苦痛であり、これぐらい自動的に実装して欲しいというクレームが多数寄せられることになる。そこで、直和と直積に閉じた型クラスで、標準で提供されているものに関しては、多くの場合自動的にインスタンスを実装してくれる機能が用意されている。それが、deriving だ。先ほどのプログラムは、以下のように書くこともできる:

.. code-block:: haskell

  data D1
    = D1_C1 Int D1
    | D1_C2 Bool
    deriving Eq

  data D2
    = D2_C1 Int D2
    | D2_C2 Bool
    deriving Eq

随分と記述量が減り、プログラムとしても見やすくなっただろう。具体的に deriving が使える型クラスはどう言うものがあるのかについては、後述する。

ところで、このようなインスタンス導出のやり方は、型クラス決め打ちのものであり、それぞれの型クラスに対してコンパイラが導出アルゴリズムを用意してあげなければならない。しかし、それはあまりに拡張性がない。そこで、GHC では標準の導出戦略の他に3つの導出戦略を追加し、ユーザが一定のコードを用意さえすれば、それを利用してインスタンスを自動で導出するように拡張を加えている。それぞれの戦略は、

``stock``
  標準の導出戦略で、型クラスそれぞれに対しコンパイラが導出アルゴリズムを用意し、それを使用して導出を行う。

``newtype``
  ``newtype`` で宣言したデータ型に対して使える戦略で、元の型のインスタンス実装をそのまま流用する。

``anyclass``
  インスタンス実装を空の実装として導出する。型クラスにデフォルトの実装が用意されてる場合、それが利用される。

``via``
  指定されたキャストできる範囲のデータ型のインスタンスを、流用する。

なお、型クラスとデータ型の組み合わせによって、使用できる戦略が複数ある場合がある。その場合に使用する戦略を明示的に書くことを可能にする拡張も用意されている。それが、``DerivingStrategies`` という拡張である。この拡張下では、先ほどのプログラムは次のように書ける:

.. code-block:: haskell

  data D1
    = D1_C1 Int D1
    | D1_C2 Bool
    deriving stock Eq

  data D2
    = D2_C1 Int D2
    | D2_C2 Bool
    deriving stock Eq

なお、何も指定しない場合は、GHC が規定する `解決の流れ <https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/deriving-strategies#the-deriving-strategy-resolution-algorithm>`_ に沿って解決が行われる。ただ、基本的には戦略を明示するのがいいだろう [#notice-ghc-88]_ 。以降は、この拡張を有効にしたとして、戦略を明示して書くようにする。では、3つの戦略と標準の導出戦略の詳細を見ていこうと思う。

DerivingVia による導出
----------------------

.. code-block:: haskell

  data MyMaybe a
    = MyJust a
    | MyNothing

このデータ型に対して ``Semigroup`` のインスタンス実装をしたいとする。この時、インスタンス候補は幾つかある。まず、自明なインスタンスは以下のようなものだ:

.. code-block:: haskell

  instance Semigroup (MyMaybe a) where
    _ <> x = x

もう一つ自明なものがある:

.. code-block:: haskell

  instance Semigroup (MyMaybe a) where
    x <> _ = x

他にも、例えば ``Applicative`` のインスタンスがあれば、次のように書ける:

.. code-block:: haskell

  instance Functor MyMaybe where ...
  instance Applicative MyMaybe where ...

  instance Semigroup a => Semigroup (MyMaybe a) where
    x1 <> x2 = (<>) <$> x1 <*> x2

さらに ``Alternative`` のインスタンスがあれば、次のように書ける:

.. code-block:: haskell

  instance Alternative MyMaybe where ...

  instance Semigroup (MyMaybe a) where
    x1 <> x2 = x1 <|> x2

これらのインスタンス実装は、``MyMaybe`` 固有のものではなく、条件さえ満たしていれば他のデータ型にも流用できそうである。また、インスタンス実装が、このデータ型固有のものではなく、共通の自然な導出によるものであることを明示できれば、それはコードを管理する上でもアドバンテージになるだろう。``DerivingVia`` はまさにそのような導出戦略を明示できる機能だ。上記のインスタンス実装は、``DerivingVia`` を使えばそれぞれ次のように書ける:

.. code-block:: haskell

  {-# LANGUAGE DerivingVia #-}

  import Data.Semigroup
  import Data.Monoid

  data MyMaybe a = ...
    deriving Semigroup via Last (MyMaybe a)

  data MyMaybe a = ...
    deriving Semigroup via First (MyMaybe a)

  data MyMaybe a = ...
    deriving Semigroup via Ap MyMaybe a

  data MyMaybe a = ...
    deriving Semigroup via Alt MyMaybe a

これは、気持ち的にはそれぞれ次のようにインスタンスを生成する:

.. code-block:: haskell

  {-# LANGUAGE TypeApplications    #-}
  {-# LANGUAGE ScopedTypeVariables #-}

  import Data.Coerce

  instance Semigroup (Last (MyMaybe a)) => Semigroup (MyMaybe a) where
    (<>) = coerce $ (<>) @(Last (MyMaybe a))

  instance Semigroup (First (MyMaybe a)) => Semigroup (MyMaybe a) where
    (<>) = coerce $ (<>) @(First (MyMaybe a))

  instance Semigroup (Ap MyMaybe a) => Semigroup (MyMaybe a) where
    (<>) = coerce $ (<>) @(Ap MyMaybe a)

  instance Semigroup (Alt MyMaybe a) => Semigroup (MyMaybe a) where
    (<>) = coerce $ (<>) @(Alt MyMaybe a)

気持ち的にはと言ったのは、最終的に制約部分が単純化され、最低限の制約に展開されるからだ [#notice-semigroup-has-multi-methods]_ 。例えば、``Semigroup (Last (MyMaybe a))`` や ``Semigroup (First (MyMaybe a))`` は常に制約が満たされるので除去され、``Semigroup (Ap MyMaybe a)`` や ``Semigroup (Alt MyMaybe a)`` はそれぞれ ``(Applicative MyMaybe, Semigroup a)``、``Alternative MyMaybe`` に展開される。さらに単純化によって、``Applicative MyMaybe`` や ``Alternative MyMaybe`` はその場で計算され、インスタンスがない場合はエラーになる。なお、単純化の計算結果を使いたくない場合、``StandaloneDeriving`` を使うと良い。``StandaloneDeriving`` はインスタンス導出を、データ型宣言とは独立にできる GHC 拡張で、インスタンスのヘッド部分を明示することができる。今回の場合は、上の ``DerivingVia`` を使った例は次のようにも書ける:

.. code-block:: haskell

  {-# LANGUAGE StandaloneDeriving #-}

  deriving via Last (MyMaybe a) instance Semigroup (MyMaybe a)

  deriving via First (MyMaybe a) instance Semigroup (MyMaybe a)

  deriving via Ap MyMaybe a instance Semigroup a => Semigroup (MyMaybe a)

  -- または、推奨されないが Applicative インスタンスの判定を、次のように遅延させることも可能
  deriving via Ap MyMaybe a instance (Applicative MyMaybe, Semigroup a) => Semigroup (MyMaybe a)

  deriving via Alt MyMaybe a instance Semigroup (MyMaybe a)

  -- または、Ap と同じく Alternative インスタンスの判定を、次のように遅延させることも可能
  deriving via Alt MyMaybe a instance Alternative MyMaybe => Semigroup (MyMaybe a)

``StandaloneDeriving`` を使う場合は、制約の単純化は必要ないため、``DerivingVia`` が行うことは、主に ``coerce`` によって実装元から実装を持ってくることになる。もちろん、``coerce`` できないものから実装を持ってくることはできないので、例えば次のようなことは書けない:

.. code-block:: haskell

  deriving via Maybe a instance Semigroup a => Semigroup (MyMaybe a)

この場合、

.. code-block:: haskell

  instance Semigroup a => Semigroup (MyMaybe a) where
    (<>) = coerce $ (<>) @(Maybe a)

という実装が作られることになり、``Coercible (Maybe a -> Maybe a -> Maybe a) (MyMaybe a -> MyMaybe a -> MyMaybe a)`` の制約が満たされなければならない。``(->)`` は両方 ``representation`` ロールを持つ [#notice-runtimereps]_ ので、この制約は ``Coercible (Maybe a) (MyMaybe a)`` が成り立つかによって決まる。そして、この制約は残念ながら成り立たない。よって、上記のインスタンス実装は型検査に失敗する。

このように ``DerivingVia`` は全ての継承できそうなインスタンスを継承できるわけではないが、かなり広い範囲のインスタンスの自動実装が可能だ。しかも、その仕組みはみた通りかなり単純だ。もし、あるクラスがインスタンスを自動で導出できそうな状況であれば、

1. そのインスタンスを実装する ``newtype`` を作る
2. その ``newtype`` を元に、``DerivingVia`` を使う

という手順で、ユーザ定義により自動でインスタンス導出を行うことができる。この手順は通常の Haskell プログラミングからそうかけ離れたものでなく、マクロなどのメタプログラミングも必要ない、かなり強力な機能だ。さらには、``DerivingVia`` は他の導出戦略を模倣可能だ。では、他の戦略を見ていこう。

GeneralizedNewtypeDeriving (GND) による導出
-------------------------------------------

``newtype`` 戦略は、``newtype`` によって定義されたデータ型に特化した戦略だ。この戦略は、``GeneralizedNewtypeDeriving``、通称 GND という拡張で利用可能だ。この戦略を使って、

.. code-block:: haskell

  {-# LANGUAGE GeneralizedNewtypeDeriving #-}

  newtype MyMaybe a = MyMaybe (Maybe a)
    deriving newtype Semigroup

と書くと、気分的には次のようなインスタンス実装が作られる:

.. code-block:: haskell

  instance Semigroup (Maybe a) => Semigroup (MyMaybe a) where
    (<>) = coerce $ (<>) @(Maybe a)

なお、制約部分は、最終的に ``DerivingVia`` の時と同じく単純化され、``Semigroup a`` に置き換わる。なお、``StandaloneDeriving`` で遅延できる話も同じだ。さらに、インスタンス実装の形も良くにている。実際、``newtype`` 戦略は、``DerivingVia`` で簡単に書き直せる:

.. code-block:: haskell

  {-# LANGUAGE DerivingVia #-}

  newtype MyMaybe a = MyMaybe (Maybe a)
    deriving Semigroup via Maybe a

見ての通りそのままになる。冗長ではあるが、この方がどのデータ型のインスタンスを流用するのかが分かりやすいと思う。これについては、いいだろう。GND は ``DerivingVia`` の特殊なバージョンということができるだろう。

DeriveAnyClass による導出
-------------------------

さて、Haskell 業界には、一切メソッドを実装しなくても、自動的にメソッドを作ってくれるクラスがいくつかある。代表的なものが ``Hashable`` クラスだ。``Hashable`` クラスは、``hashable`` パッケージで提供されているクラスで、``unordered-containers`` パッケージなどハッシュ値の計算を必要とするデータ構造を提供するパッケージにおいて、ハッシュ値を計算できるクラスとして用いられている。ただ、ハッシュ値の計算自体は冗長なのに対しほぼボイラープレート化することが多い。そこで、デフォルトで ``Generic`` のインスタンスがあれば自動でハッシュ値の計算方法を実装してくれるようになっている。このため、ユーザは特になんのメソッドも書かずにインスタンスを定義できる。具体的には、次のようにだ:

.. code-block:: haskell

  data A = ...

  instance Hashable A

なお、このインスタンス実装は、導出戦略としても使える。それを行うのが、``anyclass`` 戦略だ。``anyclass`` 戦略は ``DeriveAnyClass`` 拡張で利用可能になっており、次のように書ける:

.. code-block:: haskell

  {-# LANGUAGE DeriveAnyClass #-}

  data A = ...
    deriving anyclass Hashable

なお、名前の由来は、この戦略がその名の通りどのクラスに対しても適用可能なことから来ている。実は、Haskell はどのクラスに対しても空の実装を持つインスタンスが書ける。例えそのクラスが実装を必要とするメソッドを持っていたとしてもだ。例えば、次のようなことが書ける:

.. code-block:: haskell

  data A = ...

  instance Semigroup A

このコードは、概ね次のように展開される:

.. code-block:: haskell

  data A = ...

  instance Semigroup A where
    (<>) = undefined

実際には、もう少し丁寧なエラーメッセージが付いたりするが、実行時エラーになるのは変わらない。``anyclass`` 戦略は、これと同じことをするので、上記のことは次のようにも書ける:

.. code-block:: haskell

  data A = ...
    deriving anyclass Semigroup

ただこれはあまりにもあんまりなので、GHC では独自に実装するべきメソッドが管理されていて、そのメソッドが実装されていないと警告が出るようになっている。

さて、実装がデータ型によらず決まるというのは、``DerivingVia`` お得意というかそもそもそういう時のために生まれてきたという状況で、``anyclass`` 戦略は ``DerivingVia`` で自然に模倣できる。例えば、``Hashable`` の場合、

.. code-block:: haskell

  newtype FromGeneric' r a = FromGeneric a
  type FromGeneric a = FromGeneric' (Rep a) a

  instance (Generic a, Rep a ~ r, CanDeriveHashable r) => Hashable (FromGeneric' r a) where
    ...

というようなフレームワークを用意しておけば、

.. code-block:: haskell

  data A = ...
    deriving stock Generic
    deriving Hashable via FromGeneric A

と書ける。こちらの方が、どういうインスタンスの導出戦略を使っているかが明示的で分かりやすいだろう。なお、もちろん、危険な方のインスタンス導出も、次のように模倣できる:

.. code-block:: haskell

  newtype AnyClass a = AnyClass a

  instance Semigroup (AnyClass a)

  data A = ...
    deriving Semigroup via AnyClass A

ただ、このような使用用途はもちろん推奨されない。

``anyclass`` 戦略では危険な導出戦略と安全な導出戦略が混在していたわけで、それを見分ける方法は警告だけだったわけだが、``DerivingVia`` では元とする型によってそれをきっちりと分けることができる。さらに、``anyclass`` を前提とする場合デフォルト実装は一つしか提供できない。ところが、``DerivingVia`` はいくらでも戦略を増やすことができる。ハッシュ値の計算は、幾つかアルゴリズムの選択肢があるが、``DerivingVia`` と型レベルプログラミングを使えばどれを使用するか自由に選べたりする。なお、``anyclass`` 戦略でできて ``via`` 戦略ではできない例は思いついていないんだけど、これはもしかしたらあるかもしれない [#conjecture-anyclass-by-via]_ 。誰か知ってたら教えて欲しい。大半は ``via`` 戦略で模倣できると思うので、``anyclass`` 戦略もやはり ``via`` 戦略の特殊なバージョンと思えるだろう。

標準の導出戦略
--------------

最後は、``stock`` 戦略の紹介になる。と言っても、これに関してはあまり言うことはない。``deriving Eq`` とか書いておくと、GHC が勝手にどうやってるのか知らんけどインスタンス実装してくれるやつである。最初に触れたように、これは基本的に直積と直和からインスタンスを類推している。なお、``StandaloneDeriving`` を使わない場合は、型検査が自明に通るようなインスタンスしか実装されず、``DerivingVia`` などに比べコンパイル時間も削減できたりする。標準の範囲では、次のクラスが ``stock`` 戦略で導出可能だ:

``Eq``
  等価性判定ができるクラス

``Ord``
  順序比較ができるクラス

``Enum``
  整列ができるクラス

``Bounded``
  上限と下限があるクラス

``Show``
  文字列表示ができるクラス

``Read``
  文字列からの読み込みができるクラス

``Ix``
  上下を設定した範囲内で整列ができるクラス

GHC では他に、幾つかのクラスの導出ができるようになっている。GHC 8.8 現在で追加されているクラスは、以下の通りだ [#notice-derive-data-typeable]_ :

``Data``
  型を超えてデータを一般的に扱うことができるクラス。``DeriveDataTypeable`` 拡張が必要。

``Generic`` / ``Generic1``
  データ型のメタ情報が取得できるクラス。``DeriveGeneric`` 拡張が必要。

``Foldable``
  リストへの変換が可能なクラス。``DeriveFoldable`` 拡張が必要。

``Functor``
  任意の関数を対象の高次なデータ型への関数へと持ち上げることができるクラス。``DeriveFunctor`` 拡張が必要。

``Traversable``
  文脈付きの探索ができるクラス。``DeriveTraversable`` 拡張が必要。

``Lift``
  TemplateHaskell 上の構文木に変換できるクラス。``DeriveLift`` 拡張が必要。

ここで挙げたクラスを導出したい場合、特に何かこだわりがあるわけでなく、``stock`` 戦略で導出可能なら、自前でインスタンスを書かず ``stock`` 戦略を使うことをお勧めする。これは、無用な混乱を避けるためだ。

ところで、``stock`` 戦略があるのであまり実益としての意味はないのだが、``stock`` 戦略の多くは ``Generic`` と ``DerivingVia`` で模倣できる。例えば、``Eq`` クラスの導出は次のように書ける:

.. code-block:: haskell

  {-# LANGUAGE ScopedTypeVariables #-}

  newtype FromGeneric' r a = FromGeneric a
  type FromGeneric a = FromGeneric' (Rep a) a

  instance (Generic a, r ~ Rep a, Eq (r ())) => Eq (FromGeneric' r a) where
    FromGeneric x1 == FromGeneric x2 = from x1 == (from x2 :: r ())

  data A = ...
    deriving stock Generic
    deriving Eq via FromGeneric A

``stock`` 戦略に比べ変換を一度かます必要はあるものの、実際の計算フロー的には ``stock`` 戦略で作られたインスタンスと上のインスタンスは、同じことをしている。``Ord`` やちょっと工夫は必要だが他の標準のクラスも同じような方法で導出するシステムを作れるし、GHC で用意されてるものも ``Generic1`` から作れる。ここから分かるように、``DerivingVia`` は結構強力な機能になる。

おまけ: デフォルト実装を模倣する
--------------------------------

さて、Haskell にはクラスメソッドのデフォルト実装という機能がある。インスタンス定義において、未実装のメソッドは、デフォルト実装が用意されていればその実装で埋められ、無ければ実行時エラーが埋められる。デフォルト実装は、通常実装する必要のあるメソッドを使って実装されることが多い。例えば、今まで誤魔化してきたのだが、``Semigroup`` は実際には3つのメソッドを持ち、次のように定義される:

.. code-block:: haskell

  class Semigroup a where
    (<>) :: a -> a -> a

    sconcat :: NonEmpty a -> a
    sconcat (a :| as) = go a as where
      go b (c:cs) = b <> go c cs
      go b []     = b

    stimes :: Integral b => b -> a -> a
    stimes = ...

``stimes`` のデフォルト実装はちょっと複雑になるので省略する。``sconcat`` のデフォルト実装を見ると分かる通り、``(<>)`` を定義内で使っているが、後は依存が完結している。よって、``(<>)`` の実装さえあれば、この実装は動くようになっている。ところで、``deriving`` とは少々外れるが、このようなデフォルト実装を ``DerivingVia`` で模倣することもできる。次のようにだ:

.. code-block:: haskell

  {-# LANGUAGE ScopedTypeVariables #-}
  {-# LANGUAGE TypeApplications    #-}

  class SemigroupMinimal a where
    minimalAppend :: a -> a -> a

  newtype FromMinimal a = FromMinimal a

  instance SemigroupMinimal a => Semigroup (FromMinimal a) where
    (<>) = coerce $ minimalAppend @a

    sconcat = coerce $ sconcatDefault @a
    stimes = coerce $ stimeDefault @a

  sconcatDefault :: SemigroupMinimal a => NonEmpty a -> a
  sconcatDefault = ...

  stimesDefault :: (SemigroupMinimal a, Integral b) => b -> a -> a
  stimesDefault = ...

このフレームワークを使えば、

.. code-block:: haskell

  data A = ...

  instance Semigroup A where
    (<>) = ...

は、

.. code-block:: haskell

  data A = ...
    deriving Semigroup via FromMinimal A

  instance SemigroupMinimal A where
    minimalAppend = ...

と書ける。むしろ冗長かしていてボイラープレートは増えているが、その代わり暗黙的なデフォルト実装は明示できる。また、デフォルト実装は一通りの実装パターンしか選べないが、他にもデフォルト実装のパターンがある場合、このような工夫は生きてくる。その場合、``DerivingVia`` でどのデフォルト実装セットを使うか選べるからだ。また、デフォルト実装のパターンは後からいくらでも追加できるし、サードパーティの人も追加できるようになる。このように単純な仕組みに対して、実装の可能性がかなり広がるのが、``DerivingVia`` の強力なところだと思う。

まとめ
------

というわけで、各導出戦略の紹介、もとい ``DerivingVia`` の紹介でした。これはかなり未来感じる機能だと思うし、少なくとも ``newtype`` / ``anyclass`` 戦略はもう必要なくない？ みたいな気持ちでいる。

なお、デフォルト実装も上手く扱えるよう ``DerivingVia`` を拡張するのはどうすればいいかな的なことは、たまに考えてる。``DerivingVia`` の前後、つまり実装に必要なメソッド定義を与えるのと、``DerivingVia`` で定義されたメソッドを上書きするみたいな機能があると、もっと柔軟になりそうだけど、それはそれで機能過多かもしれないとも思う。``deriving`` 自体は、インスタンス実装を隠蔽できるという性質も持っていて、``DerivingVia`` はさらにその機能を強力にできるという側面もある。まあ、色々可能性ひろがリングね。てことで、今回は以上。

.. [#notice-ghc-88] なお、GHC 8.10 からは戦略を指定しないで ``newtype`` / ``anyclass`` 戦略を使おうとすると、警告が出るようになった。
.. [#notice-semigroup-has-multi-methods] さらに厳密に言えば、``coerce`` されるのは ``(<>)`` メソッドだけではないというのもある。``Semigroup`` は他に ``sconcat``、``stimes`` メソッドを持っているので、これらも ``coerce`` される。ただ、ここではそれらを省略している。
.. [#notice-runtimereps] 厳密には、``(->)`` は GHC 8.8 現在だと4つの引数を持ち、``(->):: (q :: RuntimeRep) -> (r :: RuntimeRep) -> (a :: TYPE q) -> (b :: TYPE r) -> Type`` という種をしている。このうち、最初の ``q``、``r`` が ``nominal`` で、次の ``a``、``b`` が ``representational`` である。
.. [#conjecture-anyclass-by-via] おそらくないと思う。
.. [#notice-derive-data-typeable] 実はもう一個 ``Typeable`` と言うクラスが、``DeriveDataTypeable`` で導出可能だ。しかし、ある問題からこれは非推奨になっている。詳しくは、https://gitlab.haskell.org/ghc/ghc/-/wikis/typeable を覗いてみるといいと思う。なお、現状全てのデータ型は ``Typeable`` インスタンスが自動で作られるようになっており、独自にインスタンスを書いたり ``deriving`` を使ったりしても完全に無視される。
