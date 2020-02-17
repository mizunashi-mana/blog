AtCoder の新環境テストの問題を解いてみる
========================================

:date: 2020-02-17 20:00
:tags: Haskell, GHC, AtCoder, 競技プログラミング
:category: 学習

AtCoder が現在新環境のテストをしている: https://atcoder.jp/contests/language-test-202001

今までの環境では GHC 7.10.3 だったのが，新環境では 8.6.5 になるので，この機会に参加してみるかということで，テストで公開されてる問題を解いてみた．そのコードと解説．なお，当方 AtCoder の Haskell 経験全然無いので，なんか指摘あったらしてくれ．

テンプレ作成
------------

よく知られてる問題として，Haskell で AtCoder の問題解く場合，``Prelude`` を単純に使うと全部 ``String`` でデータを持ってきてしまいめっちゃメモリ食って GC のお世話になると言うのがある．他にも AtCoder の問題だと気軽にソートしたい時とかあるけど，リストのソートめっちゃ遅かったり，とにかく ``Prelude`` はめっちゃリスト推してくるんだけど，現実的にリストだと辛い場面が多い．なので，``ByteString`` や ``Vector`` 系の API を整備したくなってくるので，整備した．

整備したのは， https://github.com/mizunashi-mana/haskell-atcoder-template に置いてある．もし，ちゃんと ARC / ABC に参加してたら，不足してるのどんどん足してくかもしれない．

具体的に何整備したかと言うと，まずよく使う言語拡張のオプション追加した:

``BangPatterns``
  bang pattern ``!`` が使えるようになる拡張．これつけるとパターンを一回 ``seq`` 挟んでから照合するようになる．これ付けとくと，最適化の解析がちょっと速くなって，情報も増えるので通常より最適か効きやすくなって速くなる場合が多い．ただ，後述する ``Strict`` で基本は付くのであまり使わなくて良い．ただ，ネストしたパターンについては，``Strict`` じゃダメな場合があるので，明示的に bang pattern 書く必要がある

``BlockArguments``
  ``f $ \x -> x`` とかを ``f \x -> x`` って書けるやつ．いらない ``$`` 省けるのでタイプ数が 1 減る

``FlexibleContexts``
  型制約の記法をちょっと柔軟にするやつ．普通 ``f :: Enum Int => Int`` とか書けないところを書けるようにしてくれる．元々の制限は型推論で出てきた型にも適用されるので，``Vector`` の API とかでたまにこの拡張が必要になる

``LambdaCase``
  ``\x -> case x of ...`` を ``\case ...`` って書けるやつ．モナディックなやつ書くときに ``do { r <- m; case r of ... }`` を ``m >>= \case ...`` みたいに書ける

``MultiWayIf``
  ``if cond1 then e1 else if cond2 then e2 else e3`` を

  .. code-block:: haskell

    if
      | cond1     -> e1
      | cond2     -> e2
      | otherwise -> e3

  って書けるやつ．ネストをしないで済みやすい

``OverloadedLists``
  ``[x :: a, y, z]`` が ``(IsList l, Item l ~ a) => l`` みたいな型を持つようになるやつ． ``Vector`` 気軽に作りたいとき便利

``OverloadedStrings``
  ``"str"`` が ``IsString p => p`` みたいな型を持つようになるやつ． ``ByteString`` 気軽に作りたいとき便利

``ScopedTypeVariables``
  トップレベルの型注釈で ``forall a. a -> a`` みたいな書き方ができるようになって，型 ``a`` を式中で参照できるようになるやつ．後述の ``TypeApplications`` と合わせて使うと便利

``Strict``
  全てのパターンの外側に bang pattern ``!`` が付くようになるやつ．bang pattern いちいち付けても同じ恩恵受けられるけど，めんどいし忘れる場合も多いのでこの拡張使うと良い

``TypeApplications``
  ``x :: forall a b. a -> Int -> b`` みたいなのに対して，``x @Char @() :: Char -> Int -> ()`` みたいに型適用できるやつ． ``read :: forall a. Read a => String -> a`` 系の出力が多相的なやつは曖昧な型でエラーになりやすいが，``read @Int`` みたいにしておくとわざわざ注釈書かなくても型を決められる

それから入出力系を整備した．まず， ``Read`` に変わるやつで， ``ReadBS`` っていうの定義してる．これは単純に ``ByteString`` から読み込むやつ．で，それベースに

``readLineInputs :: forall a. ReadBS a => IO [a]``
  一行読み込んで，空白区切りで分けて，それぞれ読み込むやつ

``readLineInputsVec :: forall a. ReadBS a => UVec.Unbox a => IO (Vector a)``
  ``readLineInputs`` とやることは同じだけど，返り値がリストじゃなくて unboxed vector

``discardLine :: IO ()``
  一行捨てる

``printN :: Show a => a -> IO ()``
  改行なし ``print``

とかを定義した．後， ``Vector`` / ``MVector`` をそれぞれ unboxed vector / unboxed mutable vector のエイリアスに設定してたり，``Debug`` 空間を ``Debug.Trace`` のエイリアスにしたり，諸々小細工したりしてる．詳細は https://github.com/mizunashi-mana/haskell-atcoder-template/blob/master/src/Header.hs を見てくれ．

で，こいつを ``CPP`` で ``#include`` して使ってる．

Language Test の解答例
----------------------

で，このテンプレを使った解答例を挙げていく．入出力の概略ぐらいしか問題文は書かないので，`AtCoder の問題ページ <https://atcoder.jp/contests/language-test-202001/tasks>`_ も参照してくれ．

Welcome To AtCoder
::::::::::::::::::

入力
  ::

    a
    b c
    s

  ``a``，``b``，``c`` は整数値，``s`` は文字列

出力
  ``a + b + c`` と ``s`` を空白区切りで一行に

.. code-block:: haskell

  #include "../src/Header.hs"

  main :: IO ()
  main = do
    [x] <- readLineInputs @Int
    [y, z] <- readLineInputs @Int
    s <- BS.getLine

    printN $ x + y + z
    putSpace
    BS.putStrLn s

これはいいと思う．type application が無いと，入力 ``x``，``y``，``z`` が ``Num a => a`` ぐらいまでしか決まらなくて，type defaulting が起きる．``-Wall`` 下だと警告が出るので ``Int`` を指定してる．

Product
:::::::

入力
  ::

    a b

  ``a``，``b`` は整数値

出力
  ``a * b`` が奇数なら ``Odd`` と，偶数なら ``Even`` と出力

.. code-block:: haskell

  #include "../src/Header.hs"

  main :: IO ()
  main = do
    [x, y] <- readLineInputs

    putStrLn $ solve x y

  solve :: Int -> Int -> String
  solve x y
    | (x * y) `mod` 2 == 0 = "Even"
    | otherwise            = "Odd"

これも良いと思う．特に言うことはなさそう．

Placing Marbles
:::::::::::::::

入力
  ::

    abc

  ``a``，``b``，``c`` は ``0`` か ``1``

出力
  ``1`` の個数

.. code-block:: haskell

  #include "../src/Header.hs"

  main :: IO ()
  main = do
    s <- Text.getLine
    print $ ocount (== '1') s

``ocount :: MonoFoldable mono => (Element mono -> Bool) -> mono -> Int`` は ``ofoldl'`` で該当する要素を数える関数．微妙になかったので作った．

Shift only
::::::::::

入力
  ::

    N
    a1 ... aN

  ``N`` は整数で，``a1`` から ``aN`` も整数

出力
  最大何回 ``a1`` から ``aN`` を 2 で割れるか

.. code-block:: haskell

  #include "../src/Header.hs"

  main :: IO ()
  main = do
    discardLine
    xs <- readLineInputs

    print $ minimum [ checkShifts x | x <- xs ]

  -- |
  --
  -- >>> checkShifts 0
  -- 0
  -- >>> checkShifts 2
  -- 1
  -- >>> checkShifts 20
  -- 2
  --
  checkShifts :: Int -> Int
  checkShifts = go 0
    where
      go m 0 = m
      go m n
        | n .&. 1 == 1 = m
        | otherwise    = go (m + 1) $ n `shiftR` 1

テンプレでは ``Data.Bit`` が読み込まれてて使える．それ使って，それぞれ実際割り切れなくなるまで割ってみて，その中で一番早く割り切れなくなったやつを持ってくる．まあ，アルゴリズム的な最適化の余地は幾つかあるけど，いいでしょ．

Coins
:::::

入力
  ::

    a
    b
    c
    x

  ``a``，``b``，``c``，``x`` は整数

出力
  500 が ``a`` 個，100 が ``b`` 個，50 が ``c`` 個ある状況で，ちょうど ``x`` になるような組み合わせの数

.. code-block:: haskell

  #include "../src/Header.hs"

  main :: IO ()
  main = do
    [a] <- readLineInputs
    [b] <- readLineInputs
    [c] <- readLineInputs
    [x] <- readLineInputs

    print $ solve a b c x

  -- |
  --
  -- >>> solve 1 1 1 0
  -- 1
  --
  solve :: Int -> Int -> Int -> Int -> Int
  solve a b c x = length @[] do
    a' <- [0..a]
    b' <- [0..b]
    let x' = x - a' * 500 - b' * 100
    withFilter $ x' >= 0 && x' <= c * 50
    pure ()

``withFilter`` は Scala からの輸入で，リスト内包表記の条件式相当のやつ．基本的には 500 / 100 の範囲で全探索してる．ちゃんとやろうと思えばなんかできそう．``OverloadedLists`` を使ってる弊害で，リストリテラルの表記で，曖昧な型エラーが起きるので，``Foldable`` のインスタンスを type application で固定してる．

Some Sums
:::::::::

入力
  ::

    n a b

  ``n``，``a``，``b`` は整数

出力
  1 以上 ``n`` 以下で，10 進法での各桁の和が ``a`` 以上 ``b`` 以下であるものの総和

.. code-block:: haskell

  #include "../src/Header.hs"

  main :: IO ()
  main = do
    [n, a, b] <- getLineInputs @Int

    print $ sum [ x | x <- [1..n], let k = sumDigits x, a <= k, k <= b ]

  -- |
  --
  -- >>> sumDigits 11
  -- 2
  --
  sumDigits :: Int -> Int
  sumDigits = go 0
    where
      go m 0 = m
      go m n = go (m + n `mod` 10) $ n `div` 10

リスト内包表記も便利．特に ``length`` とか ``sum`` とかに食わせる場合は融合変換が効くので，気にしないで書いて良さそう．これはまあ，そのまま安直に問題文の指示通りのことをしてる．

Card Game for Two
:::::::::::::::::

入力
  ::

    N
    a1 ... aN

  ``N``，``a1`` から ``aN`` は整数

出力
  ``a1`` から ``aN`` を大きい順に2人がそれぞれ出していった時の2人の得点差

.. code-block:: haskell

  #include "../src/Header.hs"

  main :: IO ()
  main = do
    discardLine
    xs <- readLineInputsMVec @Int

    r <- solve xs
    print r

  -- |
  --
  -- >>> Vec.unsafeThaw [3, 1] >>= solve
  -- 2
  -- >>> Vec.unsafeThaw [20, 18, 2, 18] >>= solve
  -- 18
  --
  solve :: MVector Int -> IO Int
  solve ys = do
      MVec.sortBy rcompare ys
      ifoldlMVec' go 0 ys
    where
      go m i x = m + if i .&. 1 == 0 then x else negate x

多分この問題は入力めっちゃ少ないのでそうでもないんだけど，リストのソートめっちゃばんばんメモリ使って GC のお世話になって遅くなるイメージなのでソートは基本的に mutable vector でイントロソートする方針にしてる．そのために mutable vector 向けの API もちょっと書いた．ま，そんな感じです．

Kagami Mochi
::::::::::::

入力
  ::

    N
    d1
    ...
    dN

  ``N``，``d1`` から ``dN`` は整数

出力
  ``d1`` から ``dN`` を真に小さい順に並べた時の最大の長さ

.. code-block:: haskell

  #include "../src/Header.hs"

  main :: IO ()
  main = do
    [n] <- readLineInputs @Int
    xs :: MVector Int <- MVec.replicateM n do
      [x] <- readLineInputs
      pure x

    MVec.sort xs
    xs' <- Vec.unsafeFreeze xs
    print $ olength $ Vec.uniq xs'

これもソートしてユニークするだけ．なんか，mutable vector は割と不遇でいろんな API が提供されてない (それは並行並列な世界を考えると当然なんだけど，競プロ的には辛い．もっとゆるふわな API が欲しい)．なんで，immutable vector との間を行ったり来たりする必要がある．後，入力部分は毎度おなじみ，曖昧な型を避けるため型指定していけって感じ．

Otoshidama
::::::::::

入力
  ::

    n y

  ``n``，``y`` は整数

出力
  ``n`` 個 10000 / 5000 / 1000 を使って ``y`` を作る組み合わせ．なお，作れない時は ``-1 -1 -1``

.. code-block:: haskell

  #include "../src/Header.hs"

  main :: IO ()
  main = do
    [n, y] <- readLineInputs @Int

    case solve n y of
      Nothing           -> putStrLn "-1 -1 -1"
      Just (i1, i2, i3) -> prints [i1, i2, i3]

  -- |
  --
  -- prop> maybe n (\(x, y, z) -> x + y + z) (solve n $ m * 1000) == n
  -- >>> solve 9 45000
  -- Just ...
  -- >>> solve 20 196
  -- Nothing
  --
  solve :: Int -> Int -> Maybe (Int, Int, Int)
  solve n y = headMay @[_] do
    i1 <- [0..n]
    let n2 = n - i1
    i2 <- [0..n2]
    let i3 = n2 - i2
    let y' = y - i1 * 10000 - i2 * 5000
    withFilter $ y' == i3 * 1000
    pure (i1, i2, i3)

Coins と同じく安直にやってる．所詮，:math:`2000^2` やしいけるやろ．今回も曖昧な型回避のため type application してる．type application は特例で partial signature 使えて警告も出ないようになってる．便利．

白昼夢
::::::

入力
  ::

    s

  ``s`` は文字列

出力
  ``s`` が ``(dream|dreamer|erase|eraser)*`` にマッチするか判定し，マッチするなら ``YES``，しないなら ``NO``

.. code-block:: haskell

  #include "../src/Header.hs"

  main :: IO ()
  main = do
    s <- BS.getLine

    putStrLn case solve s of
      True  -> "YES"
      False -> "NO"

  -- |
  --
  -- >>> solve "erasedream"
  -- True
  --
  solve :: ByteString -> Bool
  solve s = case parseOnlyEof p $ BS.reverse s of
      Left{}  -> False
      Right{} -> True
    where
      p = Parse.skipMany $ altconcat [ Parse.try $ Parse.string $ BS.reverse w | w <- ws ]

      ws = [ "dream", "dreamer", "erase", "eraser" ]

接頭が一致してる単語があるので，単純にパーサ書くと早食いしてしまってうまく解けないやつ．でも逆側だと曖昧さがなくなるので逆から書いたがこれは想定解なんだろうか？ ところでこの問題，正規表現があれば一発で解決するんだけど，どうやらライブラリリストから抜けてるっぽい．これは投げとくべきだったなと反省してる．今からでも間に合うんやろか？ とりあえず質問だけは投げておいた．

Traveling
:::::::::

入力
  ::

    N
    t1 x1 y1
    ...
    tN xN yN

  ``N``，``t1`` から ``tN``，``x1`` から ``xN``，``y1`` から ``yN`` は整数

出力
  時刻 ``t1`` から ``tN`` でその地点にいれるか判定し，可能なら ``Yes``，できないなら ``No``

.. code-block:: haskell

  #include "../src/Header.hs"

  main :: IO ()
  main = do
    [n] <- readLineInputs
    solve n

  solve :: Int -> IO ()
  solve m = go m 0 0 0
    where
      go :: Int -> Int -> Int -> Int -> IO ()
      go n t x y
        | n == 0    = putStrLn "Yes"
        | otherwise = do
          [t', x', y'] <- readLineInputs
          let d = abs (x - x') + abs (y - y')
          let td = t' - t
          case td >= d && (td - d) `mod` 2 == 0 of
            True  -> go (n - 1) t' x' y'
            False -> do
              replicateM_ (n - 1) discardLine
              putStrLn "No"

純粋性をかなぐり捨てて書いた．まあ，これはいいでしょ．次行ってみよう．

Interactive Sorting
:::::::::::::::::::

入出力
  ::

    n q

  ``(n, q) = (26, 1000), (26, 100), (5, 7)`` が最初に提示される

  ``q`` 回大文字アルファベット ``c1``，``c2`` について

  ::

    ? c1 c2

  を出力でき，その度に

  ::

    r

  ``r = <, >`` が返ってくる．最後に問合せ結果から分かる ``n`` 文字のアルファベットをソートした文字列 ``s`` について

  ::

    ! s

  を出力する．

.. code-block:: haskell

  #include "../src/Header.hs"

  main :: IO ()
  main = do
    [n, _] <- readLineInputs
    solve n $ Vec.iterateN n succ 'A'

  solve :: Int -> Vector Char -> IO ()
  solve n v = do
      xs1 <- Vec.unsafeThaw v
      case n of
        5  -> go1 xs1
        26 -> go2 xs1
        _  -> error $ "unexpected input:" ++ show n
      xs2 <- Vec.unsafeFreeze xs1

      putStr "! "
      oforM_ xs2 putChar
      putEndLine
    where
      go1 xs = do
        askSwap xs 0 1
        askSwap xs 2 3
        ask xs 0 2 >>= \case
          True  -> pure ()
          False -> do
            MVec.swap xs 0 2
            MVec.swap xs 1 3
        --   2 - 3
        --  /
        -- 0 - 1

        ask xs 2 4 >>= \case
          --     4
          --    /
          --   2 - 3
          --  /
          -- 0 - 1
          True  -> askSwap xs 3 4
          -- 4
          --  \
          --   2 - 3
          --  /
          -- 0 - 1
          False -> do
            askSwap xs 0 4
            MVec.swap xs 4 2
            MVec.swap xs 4 3
        -- 0 - 2 - 3 - 4
        --  \
        --   1


        ask xs 1 3 >>= \case
          -- 0 - 2 - 3 - 4
          --  \     /
          --   1 --
          True  -> askSwap xs 1 2
          -- 0 - 2 - 3 - 4
          --          \
          --           1
          False -> do
            askSwap xs 1 4
            MVec.swap xs 3 1
            MVec.swap xs 2 1

      go2 xs = sortM askChar xs

      askSwap xs i1 i2 = ask xs i1 i2 >>= \case
        True  -> pure ()
        False -> MVec.swap xs i1 i2

      ask xs i1 i2 = do
        c1 <- MVec.read xs i1
        c2 <- MVec.read xs i2
        askChar c1 c2

      askChar c1 c2 = do
        putStrLn ['?', ' ', c1, ' ', c2]
        putFlush
        [c] <- getLine
        pure case c of
          '<' -> True
          '>' -> False
          _   -> error $ "unexpected input: " ++ [c]

  -- |
  --
  -- >>> xs <- mvecFromList ['B','A','E','F','D','C']
  -- >>> sortM (\x y -> pure $ x < y) xs
  -- >>> mvecToList xs
  -- "ABCDEF"
  --
  sortM :: (Char -> Char -> IO Bool) -> MVector Char -> IO ()
  sortM cmp v = do
      v2 <- MVec.clone v
      go 0 (MVec.length v) v2 v
    where
      go i l xs ys = if
        | l == 1    -> pure ()
        | otherwise -> do
          let i1 = i
              l1 = l `div` 2
          go i1 l1 ys xs
          let i2 = i + l1
              l2 = l - l1
          go i2 l2 ys xs

          merge i1 i1 l1 i2 l2 xs ys

      merge i i1 l1 i2 l2 xs ys = if
        | l1 == 0 -> do
          let xs2 = MVec.slice i2 l2 xs
          let ys2 = MVec.slice i l2 ys
          MVec.copy ys2 xs2
        | l2 == 0 -> do
          let xs1 = MVec.slice i1 l1 xs
          let ys1 = MVec.slice i l1 ys
          MVec.copy ys1 xs1
        | otherwise -> do
          c1 <- MVec.read xs i1
          c2 <- MVec.read xs i2
          cmp c1 c2 >>= \case
            True -> do
              MVec.write ys i c1
              merge (i + 1) (i1 + 1) (l1 - 1) i2 l2 xs ys
            False -> do
              MVec.write ys i c2
              merge (i + 1) i1 l1 (i2 + 1) (l2 - 1) xs ys

率直に言ってめんどくさい．最初に ``n`` に関する場合分けをして，``n = 5`` の時は最適なソートを，それ以外の時はマージソートを行う．

``n = 5`` の時は，decision tree の葉の数が今回は ``5! = 120`` 必要で，2分木の葉の数の限界値は :math:`2^h` なので少なくとも :math:`h \geq 7` じゃないといけない．ここから最大 :math:`7` 回比較がこのソートの最適解だと分かる．逆に言えば decision tree において，選択の幅を狭めるような早めに葉に到達してしまう選択をなるべくしない比較を考えないといけない．で，まあ結果はコードの通り．なるべく対称性を保つような比較を行っていき，徐々に要素の順序を確定させていく．

マージソートの方はそのまんまって感じ．2つの mutable vector 使ってそれぞれの再帰ステップで交互に役割を交代させながらソートしてく．ソートの際 ``IO`` が必要で ``vector-algorithms`` が使えなかった．これは，テンプレにあったほうがいいんか？

モンスターテイマー
::::::::::::::::::

問題文を読み解くのがめんどくさかったので，解いてない．

まとめ
------

というわけで AtCoder 用のテンプレ作ったので，良かったら利用してください．僕もやっていきたい (やっていくとは言ってない)．DP 関連のサポートがちょっと薄いので，その辺おいおいやりながら整備していきたいねって感じ．

GHC 8.6.5 入ったら，Strict 拡張あるので，全部 unboxed literal と unboxed type 使って書く必要もなさそう．だいぶコーディング体験改善されそうで嬉しいっすね．こちらからは以上です．
