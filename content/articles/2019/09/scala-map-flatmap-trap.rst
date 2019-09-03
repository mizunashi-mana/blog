Scala の Map.flatMap の罠
=========================

:date: 2019-09-03 21:00
:tags: Scala, Collection Library, Map, 標準ライブラリ
:category: プログラミング

Scala のコレクションライブラリは難しい．最近，強くそう思える事案に出くわしたので，そのメモ．なお，使った環境は以下の通り:

+---------------------------+-------------------------------------------+
| Scala のバージョン        | 2.13.0                                    |
+---------------------------+-------------------------------------------+
| Java Runtime のバージョン | Java(TM) SE Runtime Environment 1.8.0_131 |
+---------------------------+-------------------------------------------+
| Java Hotspot のバージョン | Java HotSpot(TM) 64-Bit Server VM 25.131  |
+---------------------------+-------------------------------------------+

出くわした謎挙動
----------------

次のプログラムの結果は何になるだろうか？

.. code-block:: scala

  for {
    m <- Seq(
      Map(1 -> Seq(1, 2), 2 -> Seq(3, 4)),
      Map(3 -> Seq(), 4 -> Seq(5)),
    )
    (k, vs) <- m
    v <- vs
  } yield (k, v)

自分はこのプログラムを以下の結果が得られると期待して書いた:

.. code-block:: scala

  List((1,1), (1,2), (2,3), (2,4), (4,5)): Seq[(Int, Int)]

しかし，実際に得られるのは次の結果だ:

.. code-block:: scala

  List((1,2), (2,4), (4,5)): Seq[(Int, Int)]

このプログラムで自分が意図したことは， ``Map`` の要素の配列をフラットにして，キーと結びつけて返すことだが，何か良からぬ力が働いて，実際には ``Map`` の最後の要素しか取得できていない．

もう一問．次のプログラムの結果は何になるだろうか？

.. code-block:: scala

  for {
    m <- Seq(
      Map(1 -> Seq(1, 2), 2 -> Seq(3, 4)),
      Map(3 -> Seq(), 4 -> Seq(5)),
    )
    (_, vs) <- m
    v <- vs
  } yield v

さっきと変わったのは ``k`` を結果に使わなかっただけ．実際に動かすと，次の結果が得られる:

.. code-block:: scala

  List(1, 2, 3, 4, 5): Seq[Int]

この結果は意図通りになった．今回は全要素を全てフラットにできているようだ．さて，先ほどのプログラムの違いはなんなのだろうか？

種明かし
--------

``for-yield`` は， ``flatMap`` ， ``map`` ， ``withFilter`` (または ``filter``) を使ったコードに変換される糖衣構文だ．先ほどのプログラムは次のプログラムと一致する:

.. code-block:: scala

  Seq(
    Map(1 -> Seq(1, 2), 2 -> Seq(3, 4)),
    Map(3 -> Seq(), 4 -> Seq(5)),
  ).flatMap { m =>
    m.flatMap { case (k, vs) =>
      vs.map { v => (k, v) }
    }
  }

さて，この2個目の ``flatMap`` に注目してみる．こいつが何を返しているか調べてみる:

.. code-block:: scala

  scala> Map(1 -> Seq(1, 2), 2 -> Seq(3, 4)).flatMap { case (k, vs) => vs.map { v => (k, v) } }
  res0: scala.collection.immutable.Map[Int,Int] = Map(1 -> 2, 2 -> 4)

皆さん原因が分かったと思うが，2個目の ``flatMap`` が生成するのは ``Map`` だ．その実装は， ``IterableOnce[(K, V)]`` を受け取ると，それぞれのタプルの一つ目をキー，二つ目を値だと思って，どんどん空の ``Map`` に追加していく [#map-builder-impl]_ ．なので，キーごとに最後に書き込まれた値だけが残るというわけだ．後は， ``Map[K, V]`` 自体は ``Iterable[(K, V)]`` というトレイト条件を満たすので， ``Seq[V1]`` の ``flatMap: (V1 => IterableOnce[V2]) => Seq[V2]`` によってタプルを要素に持つ ``Seq`` データとして，結果が構築される．

ところで，二問目の結果はこれでは説明がつかない．こちらはなぜ全ての要素が消えずに残るのだろう？ 実は， ``Map[K, V]`` に対しての ``flatMap`` は，オーバーロードされた以下の2通りの実装があり，結果の型によってどちらかが選ばれる [#iterableonce-flatmap]_ ．

* ``MapOps`` の ``flatMap[K2, V2](f: ((K, V)) => IterableOnce[(K2, V2)]): Map[K, V]``
* ``IterableOps`` の ``flatMap[B](f: (K, V) => IterableOnce[B]): Iterable[B]``

一つ目のプログラムではタプルを返しているので ``MapOps`` のものが，二つ目では ``IterableOps`` のものが使われている． ``IterableOps`` のものが自分が意図したものだが，一つ目のものは ``MapOps`` の ``flatMap`` が呼ばれていたため，一見不思議な動作になったと言うわけだ．

なおこの動作につまづいた時，以下のように書き換えても同じ動作をすることが不思議だった:

.. code-block:: scala

  for {
    m <- Seq(
      Map(1 -> Seq(1, 2), 2 -> Seq(3, 4)),
      Map(3 -> Seq(), 4 -> Seq(5)),
    )
    (k, vs) <- m.toIterable
    v <- vs
  } yield (k, v)

よく分かってないのだけど， ``toIterable`` はデフォルトでは単に自身をその型のまま返すらしく， ``Map`` もそれを踏襲しているため何の意味もないっぽい．実際にこのプログラムを意図した動作にしたい場合，次のような修正が考えられる:

.. code-block:: scala

  for {
    m <- Seq(
      Map(1 -> Seq(1, 2), 2 -> Seq(3, 4)),
      Map(3 -> Seq(), 4 -> Seq(5)),
    )
    (k, vs) <- m: Iterable[(Int, Seq[Int])]
    v <- vs
  } yield (k, v)

なお，修正方法は他にもいくつかあって，とにかく ``MapOps`` の ``flatMap`` が選ばれないようにすればいいので，イテレータを返すとかでも回避できる．

まとめ
------

Scala のコレクションライブラリむずい．てかこの動作，多分一年後に正確に出力を答えられる自信がない．とりあえず， ``Map`` に対して ``for-yield`` 使ったり ``flatMap`` を他のコレクションと混ぜて使ったりするのはやめたほうが良さそうだなと思った．

後， Intellij さんの Scala プラグインが強くて，上の ``.toIterable`` とかの何の意味もないメソッドチェーン使うと灰色にしてくれたり， deprecated なメソッドチェーンは取り消し線引いてくれたりする．ただ， Scala 力が低すぎて ``.toIterable`` は何の意味もないよと Intellij さんが教えてくれてるのに，は？んなわけねえだろとか思ってたので，これからは Intellij さんを信じていきたい．こちらからは以上です．

.. [#map-builder-impl] 実際には， ``Map`` 用の可変ビルダに突っ込む．
.. [#iterableonce-flatmap] 実は ``IterableOnce`` にも ``flatMap`` が生えていて，こちらは deprecated になっている．
