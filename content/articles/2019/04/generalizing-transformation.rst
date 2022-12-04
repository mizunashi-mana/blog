Functor / Applicative / Monad が表すもの
========================================

:tags: Haskell, GHC, GHC拡張, 標準ライブラリ
:category: 直感

https://kazu-yamamoto.hatenablog.jp/entry/2019/04/11/111238 の記事に触発されて，ちょっと書く気になった．こちらも面白い記事なので，ぜひ参照してほしい．

Haskell 2020 で Applicative が追加される見込みだ (そもそもちゃんと 2020 年に出るのか怪しそうだが)．それを踏まえて，標準ライブラリに入る Haskell の特徴的な一連の型クラス Functor / Applicative / Monad の個人的な捉え方を書いておこうと思う．なお，あくまで個人的な捉え方なので，形式的でないし，広く受け入れられている考え方とは乖離している可能性があるので，そこは注意してほしい．

概要
----

それぞれの型クラスが表すものは端的に言えば，

Functor
  変換の一般化

Applicative
  関数適用の一般化

Monad
  手続きの結合の一般化

だと思う．

* 手続きの結合の特殊な場合が，関数適用 (``Monad m => Applicative m``)
* 一般的な関数適用の特殊な場合が，変換の一般化 (``Applicative m => Functor m``)

という関係になっている．なお， Haskell 2010 では ``Monad m => Functor m`` は強制されていなかったが， Haskell 2020 では ``Monad m => Applicative m`` / ``Applicative m => Functor m`` の階層がクラスに指定されるようになる [#amp-proposal-for-haskell2020]_ ．

では，それぞれの型クラスを見ていく．

Functor 型クラス
----------------

Functor クラスは，以下のようになる [#minimal-class-definition]_ :

.. code-block:: haskell

  -- |  The 'Functor' class can generalize a transformation.
  --
  -- Instances of 'Functor' should satisfy the following laws:
  --
  -- [/identity/]
  --
  --     @'fmap' 'id' = 'id'@
  --
  -- [/composition/]
  --
  --     @'fmap' (f . g) = 'fmap' f . 'fmap' g@
  --
  -- A given type has at most one valid instance of 'Functor'.
  -- And, if the identity law is satisfied, the composition law is also satisfied.
  --
  class Functor m where
    fmap :: (a -> b) -> m a -> m b

任意の変換 ``f :: a -> b`` を ``fmap f :: m a -> m b`` という変換に一般化できるのが， Functor だ．ただ，型が合ってれば何でもいいかというと，それだけではあまりにも治安が悪い．なので，コメントに書いてある通り

恒等関数の保存 (preserve identity)
  何もしない変換 (``id :: a -> a = \x -> x``) を一般化すると，やっぱり何もしない変換になる::

    fmap id = id

合成の保存 (preserve composition)
  一般化する前に合成しても，後で合成しても，結果が同じになる::

    fmap (f . g) = fmap f . fmap g

を満たす必要がある．俗に言われる ``Functor`` 則である．ただ実は，恒等関数の保存を満たせば合成の保存も自動的に満たせることが分かっている [#slim-functor-law-by-free]_ ．恒等関数の保存はモチベーションとしてはかなり理解しやすい．何もしない変換を一般化すると，意味のある変換になるというのは少し許容しがたい．そして，ただそれだけを条件に課すだけで， ``fmap`` の実装が唯一に決まるというのは不思議で面白い．これは，見過ごされがちだが， ``fmap`` が多相であることがかなり効いている．ほぼ全ての高次なデータ型は， Functor になり，その実装も唯一に決まる．

ところで， Functor に課す条件は本当にこれだけで妥当なのだろうか？ 確かに多くのデータ型で一貫した実装を提供できるということは何かしら意味を持ちそうだが，それにしても条件が，多相的で「何もしない変換を一般化すると，何もしない変換になる」というだけではそれほど強い制約にはなりそうにない．この実装が具体的に意味を持つのかは別のトピックで，横道に逸れそうなので後述の `Functor の妥当性`_ で述べることにする．少なくとも，実装が唯一に決まる変換の一般化の1つの形が， ``Functor`` であるという見方はできるだろう．ここでは，そこまで抽象的な制約に意味があるかについては，確かに議論の余地があるとだけ言っておく．

Applicative 型クラス
--------------------

Applicative クラスは，以下のようになる [#minimal-class-definition]_ :

.. code-block:: haskell

  -- |  The 'Applicative' class provides general function application.
  --
  -- Instances of 'Applicative' should satisfy the following laws:
  --
  -- [/identity/]
  --
  --      @'pure' 'id' '<*>' x = x@
  --
  -- [/composition/]
  --
  --      @'pure' (.) '<*>' u '<*>' v '<*>' w = u '<*>' (v '<*>' w)@
  --
  -- [/homomorphism/]
  --
  --      @'pure' f '<*>' 'pure' x = 'pure' (f x)@
  --
  -- [/interchange/]
  --
  --      @u '<*>' 'pure' y = 'pure' ('$' y) '<*>' u@
  --
  -- As a consequence of these laws, the 'Functor' instance for @m@ will satisfy
  --
  -- > pure f <*> x = fmap f x
  --
  class Functor m => Applicative m where
    pure :: a -> m a

    (<*>) :: m (a -> b) -> m a -> m b

Applicative クラスは，実は提唱論文では idiom と呼ばれる Applicative を用いた言語と，その為のブラケット記法というものがセットになっていた．これは今日では Applicative style と言う名前で親しまれている． Applicative style は以下の形でプログラムを書いていく手法だ::

  pure f <*> x1 <*> x2 <*> ...

この手法において必要なのは， Applicative クラスのメソッドだけだ．また GHC では取り込まれなかった為，忘れ去られてしまったが，論文中では次の記法も提唱されていた::

  [| f x1 x2 ... |]

これは上の ``pure`` と ``<*>`` で書かれた式の糖衣構文になっている．かなり関数適用の一般化として直感的な記法だ． Applicative はこの idiom に特化した Functor と Monad の中間と位置づけられたクラスだ．

もちろん Applicative もただ型があっているだけでは治安が悪いので，法則が用意されている．ただ使い方と裏腹に，その法則はかなりややこしい． Applicative が満たすべき法則， ``Applicative`` 則は以下のものになる．

恒等関数の保存 (preserve identity)
  何もしない変換 (``id :: a -> a = \x -> x``) を ``pure id :: m (a -> a)`` に埋め込んで一般化された関数適用するのは，何もしないのと同じ::

    pure id <*> x = x

  これは，ブラケット表記を使うと以下のように書ける::

    [| id x |] = x

  こうすると，関数適用の一般化という感じはある．

合成の保存 (preserve composition)
  一般化された関数に合成関数を適用した結果は，一般化された関数での合成に一致する::

    pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

  これは，ブラケット表記を使うと以下のように書ける::

    [| (\f g x -> f (g x)) u v w |] = u <*> (v <*> w)

  これも右側の ``<*>`` を無視するといい感じに関数適用の一般化をしてそうな感じは出てる．

準同型 (homomorphism)
  埋め込んだ後で一般化された関数適用をした結果と，関数適用してから埋め込んだ結果が一致する::

    pure f <*> pure x = pure (f x)

交替律 (interchange property)
  ``pure`` が右にある場合に，それを左に移すことができる::

    u <*> pure x = pure ($ x) <*> u

  右固定というのが交替っぽい．もう少し交替っぽく書くと以下の感じ::

    u <*> pure x = fmap (\f -> f x) u

  ここでは ``pure f <*> x = fmap f x`` を使っている．左が一般化された関数適用を使っていて，右が関数適用を ``fmap`` で一般化している．要は関数適用と一般化が交替していて，左は一般化してから関数適用なのが，右は関数適用してから一般化みたいな感じ．

親の ``Functor`` インスタンスとの整合性
  ``pure`` / ``<*>`` から決定される ``Functor`` の実装が，元の ``fmap`` と整合性が取れている::

    pure f <*> x = fmap f x

  ところで実は，これは前の4つが満たされていれば，自動的に満たされる．まず，

  .. code-block:: haskell

    pure id <*> x = x -- Applicative の恒等関数の保存

    (pure f <*>) . (pure g <*>) $ x
      = pure f <*> (pure g <*> x)
      = pure (.) <*> pure f <*> pure g <*> x -- Applicative の合成の保存
      = pure (f . g) <*> x                   -- 準同型
      = (pure (f . g) <*>) $ x

  から ``\f -> (pure f <*>)`` は ``Functor`` 則を満たす．さらに， ``Functor`` のインスタンスは一意なので，この実装は ``fmap`` と一致する．よって， ``pure f <*> x = fmap f x`` が成り立つことになる．

一体このような法則はどこからきたのだろうか？ 提唱論文では，実は ``Applicative`` と同じ能力を持つ別のクラスも紹介されている．それは以下のクラスだ:

.. code-block:: haskell

  -- |  The 'Monoidal' class is generalized 'Monoid'.
  --
  -- Instances of 'Monoidal' should satisfy the following laws:
  --
  -- [/naturality/]
  --
  --      @'fmap' (\(x, y) -> (f x, g y)) (u '>*<' v) = 'fmap' f u '>*<' 'fmap' g v@
  --
  -- [/left identity/]
  --
  --      @'fmap' 'snd' ('unit' >*< v) = v@
  --
  -- [/right identity/]
  --
  --      @'fmap' 'fst' (u '>*<' 'unit') = u@
  --
  -- [/associativity/]
  --
  --      @
  --      'fmap' (\((x, y), z) -> (x, y, z)) ((u '>*<' v) '>*<' w)
  --         = 'fmap' (\(x, (y, z)) -> (x, y, z)) (u '>*<' (v '>*<' w))
  --      @
  --
  class Functor m => Monoidal m where
    unit :: m ()

    (>*<) :: m a -> m b -> m (a, b)

``Applicative`` は ``<*>`` が左は一般化された関数，右は一般化された引数のように，主と従が決まっていた． ``Monoidal`` の場合，引数は同じレベルになっていて対称性がある． ``Monoidal`` から ``Applicative`` ，その逆はそれぞれ次のように書ける::

  pure x = fmap (const x) unit
  f <*> x = fmap (\(f, x) -> f x) $ f >*< x

  unit = pure ()
  x >*< y = (,) <$> x <*> y

なお，各法則が成り立つかはめんどくさいのでやらない．各自で試してみてくれ． ``Monoidal`` から ``Applicative`` の交替律を導くとこだけやっとくと，

.. code-block:: haskell

  u <*> pure x
    = fmap (\(f, x) -> f x) $ u >*< fmap (const x) unit        -- Monoidal のメソッド使って書き直し
    = ... $ fmap id u >*< fmap (const x) unit                  -- Functor の恒等関数の保存
    = ... $ fmap (\(f, ()) -> (id f, const x ())) $ u >*< unit -- naturality
    = fmap (\(f, ()) -> f x) $ u >*< unit                      -- 単純化
    = fmap ((\f -> f x) . fst) $ u >*< unit
    = fmap (\f -> f x) $ fmap fst (u >*< unit)                 -- Functor の合成の保存
    = fmap (\f -> f x) u                                       -- right identity
    = fmap (\f -> f x) $ fmap snd (unit >*< u)                 -- left identity
    = fmap ((\f -> f x) . snd) $ unit >*< u                    -- Functor の合成の保存
    = fmap (\((), f) -> f x) $ unit >*< u
    = fmap (\(g, f) -> g f) $ fmap (\((), f) -> (const ($ x) (), id f) $ unit >*< u
    = ... $ fmap (const ($ x)) unit >*< fmap id u              -- naturality
    = fmap (\(g, f) -> g f) $ fmap (const ($ x)) unit >*< u    -- Functor の恒等関数の保存
    = pure ($ x) <*> u                                         -- Applicative のメソッドに書き直し

みたいな感じになる．証明が結構対称的になっていて，中間で ``fmap ($ x) u`` が出てくる．関数適用の一般化が，こういう綺麗な構造から出てくると言うのは結構不思議だ．まあでも， ``f <*> x = fmap (\(f, x) -> f x) $ f >*< x`` という定義から考えてみると，関数適用は 1 つの変換なので ``fmap`` によって一般化できるので，後は 2 つの一般化された値 ``f :: m (a -> b)`` / ``x :: m a`` を1つの一般化された値にまとめることができれば勝ちで，実はこのまとめる操作こそ本質だったという話だろう．そして結合する話になってくると大事になってくるのが， Monoid でその法則を入れた結果，非対称な ``Applicative`` のメソッドに法則を翻訳するとぱっと見よく分からない規則が出てくると言うわけだ．

まあ実装する際に， ``Applicative`` であるが ``Monad`` でないみたいなのを実装することはあんまないはずなので，通常は法則など気にせずともいいはずだ． ``Applicative`` は使う分にはかなり直感的で， Applicative style によってまるで通常の関数適用のごとくプログラムが書ける．

Monad 型クラス
--------------

Monad クラスは，以下のようになる [#minimal-class-definition]_ :

.. code-block:: haskell

  -- |  The 'Monad' class can combine two actions.
  --
  -- Instances of 'Monad' should satisfy the following laws:
  --
  -- [/left identity/]
  --
  --      @'pure' x '>>=' k = k x@
  --
  -- [/right identity/]
  --
  --      @m '>>=' 'pure' = m@
  --
  -- [/associativity/]
  --
  --      @(m '>>=' f) '>>=' g = m '>>=' (\x -> f x '>>=' g)@
  --
  -- Furthermore, the 'Monad' and 'Applicative' operations should relate as follows:
  --
  -- > mf <*> mx = mf >>= \f -> mx >>= f
  --
  -- As a consequence of these laws, the 'Functor' instance for @m@ will satisfy
  --
  -- > fmap f x = x >>= pure . f
  --
  class Applicative m => Monad m where
    (>>=) :: m a -> (a -> m b) -> m b

``Monad`` については色んな人が記事を書いてるし，それを参照しろってことで (そろそろ飽きてきた)．

一応いくつか言っておくと，まず Haskell2020 の RFC で進んでる ``Applicative`` のプロポーサルでは， no return プロポーサルも取り込まれていて， ``return = pure`` のデフォルト実装が取り込まれる予定．後， ``join`` も ``Monad`` のメソッドとして入ることになっている．なので， ``Monad`` クラスはこうも書ける:

.. code-block:: haskell

  -- |  The 'Monad' class can combine two actions.
  --
  -- Instances of 'Monad' should satisfy the following laws:
  --
  -- [/associativity/]
  --
  --      @'join' . 'fmap' 'join' = 'join' . 'join'@
  --
  -- [/identity/]
  --
  --      @'join' . 'fmap' 'pure' = 'join' . 'pure' = 'id'@
  --
  -- Furthermore, the 'Monad' and 'Applicative' operations should relate as follows:
  --
  -- > mf <*> mx = join . fmap (\f -> fmap f mx) $ mf
  --
  class Applicative m => Monad m where
    join :: m (m a) -> m a

お馴染みの coherence condition だ． ``join . fmap join = join . join :: m (m (m a)) -> m a`` は中身を先にまとめるか外側を先にまとめるかでの coherence を表し，結合律 (associativity) と対応する． ``join . fmap pure = join . pure = id :: m a -> m a`` は中身を埋め込んでまとめるか，外側を埋め込んでまとめるかの coherence を表す．で，この合成は最終的に何もしない変換と一致し，単位元律 (identity) と対応することになる．

後 ``Monad`` が手続きの結合を表すについてだが，実は単に 2 つの独立した手続きがあった時，それを結合するのは ``Applicative`` で十分だ．手続きを繋げる時は ``const id`` を一般化して適用すればいい．例えば，

::

  pure (const id) <*> putStrLn "Hello" <*> putStrLn "World"

こうすると ``putStrLn "Hello"`` は計算されるものの結果は無視されることになる．これは， C 言語のコンマ (,) 演算子と発想は同じだ．なお， ``Applicative`` のモジュールではこれに相当する演算子 ``*>`` が用意されていて，次のように書ける::

  putStrLn "Hello" *> putStrLn "World"

Applicative が Monoidal の非対称版であることを思い出すとこの結果は納得できる． 2 つの一般化された値を 1 つの一般化された値にまとめる力は， Applicative で十分手に入るのだ．では，なぜ ``Monad`` が必要かと言うと，手続きを結合するのには 1 つの一般化された値にまとめるだけでは不十分で，前の手続きの結果を次の手続きで流用する力が必要になってくるときがあるからだ．具体的には条件分岐で，前の結果をもとに条件判断して，その判断に応じてそれぞれ別の計算を行いたい場合などだ．なので， ``Monad`` は 2 つの一般化された値を 1 つの一般化された値にまとめるのではなく，手続きを一般化された継続と見てそれをまとめる力を持つ．この点で見た時は，以下の演算子での法則の方が覚えやすい::

  f >=> g = \x -> f x >>= g

  -- identity
  f >=> pure = pure >=> f = f

  -- associativity
  (f >=> g) >=> h = f >=> (g >=> h)

``<*>`` と ``>*<`` の関係と同じく， ``>>=`` と ``>=>`` の関係は非対称 / 対称ということになる．最終的に Applicative も Monad も単位元律と結合律に落ち着いて，単に対象が違うだけと言う感じだ．

Functor の妥当性
--------------------

さて，最後に ``Functor`` クラスは妥当なのかについて触れておこうと思う．と言ってもなんかソースがあるわけでなく (てかこの記事全般そうだし，ちゃんとした記事ならブログには書かないんだよなあ) 勝手に自分で考えたことだ．

まず，現状の ``Functor`` クラスで良く見過ごされがちだがかなり大きな役割を担っているのがパラメトリック多相な ``fmap`` の定義だ．この多相性のおかげで， ``fmap`` が唯一の実装を持つことが保証される．どう言う風に証明を書くかは， https://www.schoolofhaskell.com/user/edwardk/snippets/fmap とかをのぞいてみるのがいいと思う．

実装が固定できるのはいいことなのだが，逆に制約が弱すぎて大抵の高次なデータ型は ``Functor`` を実装できてしまう．むしろ，高次のデータ型に対して一貫して提供されるユーティリティの1つと考える方が自然だと思う．事実， GHC では ``DeriveFunctor`` という拡張が提供されている．この拡張により， ``deriving`` で ``Functor`` のインスタンスを導出できる．例えば，

.. code-block:: haskell

  data RecData a
    = RecBound1
    | RecBound2 a
    | RecNode a Int (RecData a) (RecData a)

  instance Functor RecData where
    fmap f RecBound1     = RecBound1
    fmap f (RecBound2 x) = RecBound2 (f x)
    fmap f (RecNode x n r1 r2) = RecNode (f x) n (fmap f r1) (fmap f r2)

みたいなインスタンスを自動的に作ってくれる．これは，以下のかなり単純な規則で作られている:

* 変換を適用する対象に適用する．
* ``Functor`` のインスタンス部分には，変換を一般化して適用する．
* それ以外の部分はそのまま出力する．

``DeriveFunctor`` という拡張があることから分かる通り， ``Show`` や ``Eq`` 並みに広い範囲に適用できるし， free functor というのも巷では流行っていて， ``DeriveFunctor`` する代わりに同じ構造を無名で取り出すみたいなことができる．この free functor の売りが，「どんなデータ型でも ``Functor`` にできる」だから， ``Functor`` 制約がどれくらい無意味かは分かると思う．じゃあもう少し意味のある制約にできないだろうか？

例えば，単調性の保存も入れてみるとどうか？ つまり，

.. math::

  \{\forall x, y.\, x \leq y \implies f(x) \leq f(y)\}
  \implies \{\forall x, y.\, x \leq y \implies \mathrm{fmap}(f)(x) \leq \mathrm{fmap}(f)(y)\}

という法則を足す．これだと何が嬉しいかと言うと， ``f :: Int -> String = \n -> replicate n 'a'`` に対し， ``sort [fmap f n1, fmap f n2, ...]`` みたいなのを ``[fmap f n | n <- sort [n1, n2, ...]]`` とすることができる．リストの比較より ``Int`` の比較の方が効率がかなり良いことから，良い最適化になると考えられる．この法則を満たさないが，既存の ``Functor`` になるインスタンスには次のものがある:

.. code-block:: haskell

  newtype Down a = Down a
    deriving Eq

  instance Ord a => Ord (Down a) where
    compare (Down x) (Down y) = compare y x

  instance Functor Down where
    fmap f (Down x) = Down (f x)

なのでちょっと微妙かもしれない．他には， ``Foldable`` 的な方向性の拡張で， ``fmap`` は再帰的にデータを辿る方法の 1 つというのも考えられるかもしれない．実際 ``DeriveFoldable`` という拡張があるのだが，この実装方法は ``DeriveFunctor`` と良く似ている．ただ，その場合もあんまり良く考えてないけど，作り方によってはインスタンスが複数あるみたいなことが起こりそうだ．

``fmap`` が Haskell 界で広く用いられていることは，現状の ``Functor`` クラスを支持する大きな根拠となる．ただ，制約として考えた場合にもっと意味のある区分を設けたいというのは自然なことだろう．その1つの方向性が， ``Monad`` につながる， ``m a`` を ``a`` を一般化した高次のデータと見て， ``m`` が付かない世界の計算を ``m`` が付く世界にどれだけ持っていけるかを，制約として表すということだと思う．その点で， ``fmap`` は ``a`` の世界の計算を単純に ``m`` が付く世界に持っていく， ``Applicative`` / ``Monad`` を前提とした見方で使う場合が多いと思う．本来ならその見方で使うのは ``liftA`` とかなのかもしれない．

ただ単調性の保存の追加で挙げたように， ``Functor`` から制約を作っていく方向性は ``Monad`` だけではない．変換の一般化というのはもっと広い範囲で適用できて，というか出来すぎて，単体では意味がない場合が多い．でも逆に言えば，意味をかなり色んな方向から付与できる．その中でデータ型一般に適用できて，実装も単一に定まり，型クラスの仕様的にも表しやすい，最大公約数的な変換の一般化を表すクラスが，現状の ``Functor`` だとも捉えられるかもしれない．そう考えると ``Monad`` の見方で ``fmap`` を使うのも約数的な使い方ということで，ある程度妥当とも言えるかもしれない．

まとめ
------

``Applicative`` が Haskell2020 に入りそうだったので，それを記念する意味も込めて (？) ，個人的な ``Functor`` / ``Applicative`` / ``Monad`` の直感をまとめてみた．まあ，備忘録みたいな意味もあるかも．

``Applicative`` が Haskell2020 に入って， no return プロポーサルも入り，治安が良くなってきそう [#ghc-status]_ ．なお， Haskell2020 が 2020 年に出るかどうかは 🙏

.. [#slim-functor-law-by-free] https://www.schoolofhaskell.com/user/edwardk/snippets/fmap
.. [#amp-proposal-for-haskell2020] https://github.com/haskell/rfcs/pull/1
.. [#minimal-class-definition] なお，スペースと分かりやすさの都合上， minimal なメソッドだけを書くようにしている．
.. [#ghc-status] GHC には既に入ってる．
