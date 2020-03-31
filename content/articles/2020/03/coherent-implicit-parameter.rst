Coherent Implicit Parameter
===========================

:tags: Haskell, 型クラス, 型システム
:category: プログラミング言語

アドホック多相を実現する方法として，型クラスがある．型クラスは，辞書渡しと呼ばれる方法により，かなり自然に elaboration ができる．ところで，この elaboration は暗黙的引数にも転用できることが知られている。特に、型クラスを搭載した Haskell では，その処理系 GHC の内部実装を利用して暗黙的引数を実現する `reflection パッケージ <https://hackage.haskell.org/package/reflection>`_ が知られている．

ところで，暗黙的引数の仕組みは，単純に型クラスの型システムを転用するだけでは，coherence を壊してしまう．そのため，coherence を保証しながら暗黙的引数を実現する方法がいくつか研究されている．今回は，暗黙的引数と型クラスの関係，それから少し最近考えている， coherence を保って，型クラスの仕組みを少し拡張するだけで暗黙的引数を実現するアイデアを紹介したいと思う．

暗黙的引数と型クラス
--------------------

型クラスを，型クラスを持たない言語に elaboration する方法として，型クラスを暗黙的引数と見て引数を明示するようにする変換が知られている．例えば，

.. code-block:: haskell

  class C a where
    m :: a

  instance C Int where
    m = 0

  f :: C a => a
  f = m

  g :: Int
  g = f

のようなプログラムは，

.. code-block:: haskell

  data C a = C
    { m :: a
    }

  instanceC_Int :: C Int
  instanceC_Int = C
    { m = 0
    }

  f :: C a -> a
  f d = m d

  g :: Int
  g = f instanceC_Int

というように elaboration される．この方法は辞書渡し (dictionary passing) と名前で知られており，Haskell の処理系 GHC でも採用されている．GHC ではメソッドが1つの型クラスは ``newtype`` を使って elaboration される．例えば，上の例は GHC では次のように elaboration される:

.. code-block:: haskell

  newtype C a = C
    { m :: a
    }

  instanceC_Int :: C Int
  instanceC_Int = C
    { m = 0
    }

  f :: C a -> a
  f d = m d

  g :: Int
  g = f instanceC_Int

これを利用すると，次のようなプログラムが書ける:

.. code-block:: haskell

  {-# LANGUAGE ScopedTypeVariables #-}

  import Unsafe.Coerce

  newtype WrapCtx c r = WrapCtx (c => r)

  reify :: forall a r. (C a => r) -> a -> r
  reify f x = unsafeCoerce (WrapCtx f :: WrapCtx (C a) r) x

実装時表現において ``C a => r`` は ``newtype C a`` においての ``C a -> r`` と同じであり，さらにこれは ``a -> r`` と同じであるという推論から，上のコードは正常に動くであろうということになる．なお、``WrapCtx`` で包むことにより制約の評価を遅延させている。この仕組みにより，引数を省略しても自動的にコンパイラが引数を埋めてくれるシステムが出来上がる．例えば，次のように使える:

.. code-block:: haskell

  f1 :: C Int => Bool
  f1 = m == 0

  f2 :: Bool
  f2 = reify f1 10

このプログラムは，推論通りに動作するなら，次のように elaboration される:

.. code-block:: haskell

  f1 :: C Int -> Bool
  f1 d = m d == 0

  f2 :: Bool
  f2 = f1 (coerce 10)

実際には，もちろんこの動作はコンパイラに保証されてるわけでなく，推論通りにいかない可能性もある [#spj-offer]_ ．このハックを使ったライブラリとして `reflection パッケージ <https://hackage.haskell.org/package/reflection>`_ が知られている．その詳細については，`分かりやすいチュートリアル <https://www.schoolofhaskell.com/user/thoughtpolice/using-reflection>`_ があるのでそちらを参照するといいと思う．

ところで，この仕組み，次のようなコードではどう言う動きになるだろうか？:

.. code-block:: haskell

  f3 :: C Int => C Int => Int
  f3 = m

  f4 :: C Int => Int
  f4 = reify f3 10

このプログラムが想定していることは，2つの暗黙的引数があり，その内の1つに適用するというようなものだ．ただ、どちらの引数を適用するかは指定されていない。これは、未定義動作であり、どちらが適用されるかはコンパイラがどう制約を解決するかに依存している。`reflection パッケージ`_ でもこのようなものは未定義動作を引き起こすとして注意喚起がしてある。ただ、暗黙的引数として見るなら、どの引数を使うかはちゃんとプログラマが制御できて欲しい。

暗黙的引数再考
--------------------------

ところで、暗黙的引数としてのフルの機能を望まないで、とりあえずそれぞれの型に1つだけ暗黙的引数が指定できるシステムを考えてみる。この場合、上記のように複数の暗黙的引数を使っても、引数の型が同じなら同じ実体を持つため、どちらの引数を使っても結果は変わらない [#conjecture-soundness]_ 。例えば、

* ``Given a`` を暗黙的引数で ``a`` 型の値が渡ってくる制約
* ``given :: Given a => a`` を暗黙的引数として渡ってきた内容を取り出せる関数
* ``Given`` 制約はユーザによってグローバルに定義できない

とした時、

.. code-block:: haskell

  f :: Given Bool => Given Bool => Bool
  f = given

は、``Given`` 制約が2箇所あり、辞書渡しと同じ elaboration を考えるなら、実体が2つ渡ってくることになるのでどっちの辞書を採用するかが重要になるように見える。しかし、型が同じなら同じ実体を持つという制約によって、実際はどちらを採用しても ``f`` の結果は変わらない。ただ、この保証を守るため、暗黙的引数を最初に適用する部分は少し工夫が必要になる。つまり、``give`` を受け取った引数を暗黙的な引数として受け取った関数に渡すものとした時、

.. code-block:: haskell

  g :: Given Bool => Bool
  g = give True f

において、``give`` は ``f`` の全ての暗黙的引数に関する制約を解決しないといけない。なぜなら、もし解決せずに制約を残してしまうと、上位から渡ってきた ``g`` の ``Given Int`` 制約が使われてしまう可能性があり、すなわち ``f`` において ``give`` で渡されたものと上位から渡ってきたものが混在してしまい、型が同じなら同じ実体を持つという制約を守れないからだ。よって、``give`` は制約解決において特別な仕組みを持つ何かにしなければならない。

ところで今までは、型によって実体は1つに決まると制約が重要ということから、それを保証するよう既存の枠組みを修正しようと流れで話してきた。ところで、発想を転換して、そもそも実体が作られる毎に型を新規に発行するということは考えられないだろうかというのが、今回紹介するアイデアの基盤になる。新規に型を発行すれば、当然型に対して実体は一つになる。具体的には、

.. code-block:: haskell

  given :: forall k a. Given k a => Proxy# k -> a
  give :: forall a r. a -> Gift a r -> r

  newtype Gift a r = Gift (forall k. Given k a => Proxy# k -> r)

  f :: forall k1 k2. Given k1 Bool => Given k2 Bool => Proxy# k1 -> Proxy# k2 -> Bool
  f p1# p2# = given p1# && given p2#

  g :: forall k. Given k Bool => Proxy# k -> Bool
  g p2# = give True $ Gift \p1# -> f p1# p2#

みたいな感じでエミュレートができる。基本的には、``Proxy# k`` と rank 2 type がポイントになっていて、どんな鍵 ``k`` に紐づく暗黙的引数でも大丈夫な関数の場合、鍵を新規発行して引数に紐付け関数に渡す関数が ``give`` になっている。なお、``give`` / ``given`` は、`reflection パッケージ`_ の流儀に則って実装すれば実際に作れて、以下の感じになる:

.. code-block:: haskell

  class Given k a where
    given :: Proxy# k -> a

  give :: forall a r. a -> Gift a r -> r
  give x f = (unsafeCoerce f :: (Proxy# () -> a) -> Proxy# () -> r) (\_ -> x) proxy#

もちろん、``Given`` のインスタンスが作れちゃうとまずいので、そこは隠蔽する必要があるけど。なお、型の新規発行の部分は、実際には ``()`` をいつでも発行している。つまり、外部から見てどういう型を発行しているかが分からないことが大事って感じ。同じ感じのエミュレートは、存在型でも可能で、

.. code-block:: haskell

  newtype GivenInst k a = GivenInst a

  data GivenInstE a where
    GivenInstE :: GivenInst k a -> GivenInstE a

  givenInstE :: forall a. a -> GivenInstE a
  givenInstE x = GivenInstE (GivenInst x :: GivenInst () a)

  class Given k a where
    given :: Proxy# k -> a

  give :: forall k a r. GivenInst k a -> Gift k a r -> r
  give x f = (unsafeCoerce f :: (Proxy# () -> GivenInst k a) -> Proxy# () -> r) (\_ -> x) proxy#

  newtype Gift k a r = Gift (Given k a => Proxy# k -> r)

  f :: forall k1 k2. Given k1 Bool => Given k2 Bool => Proxy# k1 -> Proxy# k2 -> Bool
  f p1# p2# = given p1# && given p2#

  g :: forall k. Given k Bool => Proxy# k -> Bool
  g p2# = case givenInstE True of
    GivenInstE x -> give x $ Gift \p1# -> f p1# p2#

ぱっと見こっちの方が複雑に見えるかもしれないけど、本質的にはこっちの方がさっきのより直感的で、新規発行されたものがどういう型か言及できないように、何かよく分からん鍵付きのインスタンスが渡ってきた時に、それを暗黙的引数として型が合ってるやつに渡せる。ところで、鍵について何も言及できないということは、暗黙的引数を受け取る関数が鍵に対して多相化されてるはずで、その直感からさっきの rank 2 type による例ができる。ところで、rank 2 type の例は多相化された関数しか相手にしていないが、こっちの例は暗黙的引数の新規発行だけは特別な仕組みになっているけど、それ以外は別に鍵を多相化しなくても利用できる。なので、例えばグローバルに1つ代表的な値が作りたいとなった時、

.. code-block:: haskell

  data Global
  instance Given Global Bool where
    given _ = True

みたいなんを書いといて、これを使うみたい何ができる。また、鍵自体を、

.. code-block:: haskell

  f :: forall k. Given Global Bool => Given k Bool => Proxy# k -> Bool
  f p# = given @Global proxy# && given p#

のように指定することも可能だ。鍵が単相である分には、プロテクトは外れないので、グローバルなインスタンスがコヒーレンスを守るならそれはそれで大丈夫だ。なお、ちゃんと単相であることを保証するハックとして、

.. code-block:: haskell

  data Undefined
  instance Given Undefined a where
    given _ = undefined

と機先を制しておけば、型クラスの一意性判定により多相な鍵の登録はブロックされるはず。後は、``GivenInst`` コンストラクタを隠蔽しておけば、暗黙的引数でもグローバルにもコヒーレンスが保証される仕組みが出来上がる。

プリミティブなサポートへ
------------------------

ここまでのハックを言語機能としてサポートするための構文とかも色々と考えた。基本的には、

.. code-block:: haskell

  class C a where
    m :: a

  instance keyName :: C () where ...

  f :: C a => C a => Semigroup a => a
  f %d1 %d2 = m d1 <> m d2

ってやると、

.. code-block:: haskell

  class C k a where
    m :: Proxy# k -> a

  instance C keyName Bool where ...

  f :: C d1 a => C d2 a => Semigroup k a => Proxy# d1 -> Proxy# d2 -> Proxy# k -> a
  f d1 d2 = m d1 <> m d2

みたいなん相当になる。また、

.. code-block:: haskell

  g :: ()
  g = f %(C{ m = () }) %(C{ m = () })

みたいなんをすると、こっちは直接 Haskell では書けないけど、``%(C{ m = ()}) :: exists k. C k ()`` みたいなんが出来上がって、いい感じに elaboration される的な。コヒーレンスチェックは完全に今まで通りグローバルなものさえチェックすれば良いはず。ただ、型制約の解決と型推論はちょっと変わるかもしれない。特に、上の例は ``C d1 a`` / ``C d2 a`` のインスタンスは束縛して使ってるわけだけど、``Semigroup`` のインスタンスは束縛して使ってない。これは、``Semigroup`` のどの鍵のインスタンス使うねんって話になる場合があると思うので、そこをうまいこと定義しないといけなさそう。これは型の defaulting の仕組みと大体同じことできないかなと考えてる。多相化されたままでなるべく解決できるようにして、曖昧な鍵が残っちゃったらデフォルトの鍵を使う的な。まあ、その辺ももうちょっと話詰めないとなと考えながら、特に何も考えていないのであった。

まとめ
------

というわけで、ちょっと拡張するとコヒーレンス満たす暗黙的引数と型クラス共通の仕組み作れそうだなみたいな話でした。

直感的には、コヒーレンスの正当性示せそうな気がするけど、あまり真剣には考えてない。もしかしたら壊れがあるかも。今はあまり、新機能をちゃんとまとめて実装してみることに興味がないので、多分このアイデア自体は放置されることになるんだけど、ちゃんと覚えているうちにまとめておくのは大事な気がするので、アイデアの大筋をまとめてみた。Haskell は型で一意にしかインスタンスを作らないことを前提としていて、その拡張は大体その方針と妥協しながら何とかするみたいな方向性が多く、逆に Scala はコヒーレンスチェックをある程度捨てていて、その代わり各々で管理してくださいみたいな感じなんだけど、それぞれ両極端な感じがあるけど、その中間としてこういう方針もあるよなあと考えた感じ。

ちゃんとアイデアまとめたんだし、どうせなら近い内に実装してみて使い勝手実験できたらなあと思っている。後、ちゃんと形式的に型システムまとめて、健全性も証明しておきたいなあ。ま、そういう感じです。なお、特に自分でやりたい欲は特に無くて (他にやりたいことたくさんあるし)、使ってみたい欲しかないので、誰か実装してくれるなら実装してくれ。

.. [#spj-offer] https://www.reddit.com/r/haskell/comments/5xqozf/implicit_parameters_vs_reflection/dek9eqg/ で，GHC 側は一定のサポートをするつもりだという言説が述べられている．実際のところどうなのかは分からない．
.. [#conjecture-soundness] と考えているが正しい。厳密に健全性が示せてるわけではない。
