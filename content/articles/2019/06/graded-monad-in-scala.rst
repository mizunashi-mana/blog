Scala (Dotty) での Graded Monad によるメタ言語の実装
====================================================

:date: 2019-06-07 23:00
:tags: エフェクトシステム, モナド, Scala
:category: 構想

Graded Monad のメタ言語，普通にプログラミング言語で実装できそうやなって思って，ちょっとやってみることにした．で，前ねこはるさんが extensible effects に Dotty の union types 使う話してたの思い出して，それと同じ感じで powerset による preordered monoid に対する graded monad ぐらいなら実装できるんじゃねと思ったので， Dotty のお試しがてら実装してみることにした．

Dotty のお試しという要素が強いし， Scala 弱者なので，それほど真面目に実装してない．なんか興味ある方いたら，適当にフォークして勝手に自分で考えたことにしてくれ．なお書いたコードは， https://github.com/mizunashi-mana/graded-monad-in-scala に上げた．

Dotty のインストール
--------------------

Dotty はいつ出るか分からない， Scala の次期バージョンコンパイラ．現状の Scala の色々不便なとこが変わったり，型システムがより強力になったりするらしい．公式サイトは， https://dotty.epfl.ch/ ．お試し利用がかなり気軽にできて公式サイトにやり方が載ってる．

macOS の場合は， Homebrew で， ::

  brew install lampepfl/brew/dotty

すると入る．なお， ``sbt`` の場合は， ::

  sbt new lampepfl/dotty.g8

とすると， Dotty 用のプロジェクトが作られる．こんだけ．強い．

Graded Monad を定義する
-----------------------

Dotty には union type という型があり，型 ``X`` ， ``Y`` に対して ``X | Y`` と表記される．それぞれ，

* ``X <: (X | Y)``
* ``Y <: (X | Y)``
* ``X <: B`` かつ ``Y <: B`` ならば ``(X | Y) <: B``
* ``(X | Y) =:= (Y | X)``
* ``(X | (Y | Z)) =:= ((X | Y) | Z)``

という関係が成り立つようになっている．強い．後，こいつ， ``Nothing`` が単位元になる．

* ``(X | Nothing) =:= (Nothing | X) =:= X``

いや強すぎるやろ．ほんまかって感じだが．

この型を使って， preordered monoid :math:`(\{\text{\tt X} \mid \text{\tt X <: B}\}, \text{\tt <:}, \text{\tt |}, \text{\tt Nothing})` についての graded monad を， ``GradedMonad`` として定義することにする．とりあえず，そのまんま以下の感じで書いた:

.. code-block:: scala

  trait GradedMonad[B, T[_ <: B, _]] { self =>
    def pfunctor[E <: B]: Functor[[X] => T[E, X]]

    def gradedUpcast[E1 <: B, E2 <: B, X](m: T[E1, X]): T[E1 | E2, X]

    def gradedPure[X](x: X): T[Nothing, X]

    def gradedFlatten[E1 <: B, E2 <: B, X](m: T[E1, T[E2, X]]): T[E1 | E2, X]

    def gradedFlatMap[E1 <: B, E2 <: B, X, Y](m: T[E1, X])(f: X => T[E2, Y]): T[E1 | E2, Y] =
      gradedFlatten[E1, E2, Y](pfunctor.map(m)(f))
  }

* ``pfunctor`` と ``gradedUpcast`` が関手 :math:`T: E \to C`
* ``gradedPure`` が自然変換 :math:`\eta: \mathrm{Id} \Rightarrow T 1`
* ``gradedFlatten`` が自然変換 :math:`\mu: T - \mathbin{\otimes} T - \Rightarrow T (- \cdot -)`

に，それぞれ対応する．後， implicit 系の API も整備しておく:

.. code-block:: scala

  object GradedMonad {
    def apply[B, T[_ <: B, _]](implicit tc: GradedMonad[B, T]): GradedMonad[B, T] = tc

    def gradedPure[B, T[_ <: B, _], X](x: X)(implicit tc: GradedMonad[B, T]): T[Nothing, X] = tc.gradedPure(x)

    trait ToGradedMonadOps {
      implicit final class toGradedMonadOps[B, T[_ <: B, _], E <: B, X](
        private val tex: T[E, X]
      )(implicit tc: GradedMonad[B, T]) {
        def map[Y](f: X => Y): T[E, Y] = tc.pfunctor.map(tex)(f)

        def flatMap[E2 <: B, Y](f: X => T[E2, Y]): T[E | E2, Y] = tc.gradedFlatMap(tex)(f)

        def withFilter(p: X => Boolean): T[E, X] = tc.pfunctor.map(tex)(
          x => if p(x)
            then x
            else throw new RuntimeException("Pattern match failed: GradedMonad.withFilter")
        )

        def foreach[E2 <: B, Y](f: X => T[E2, Y]): T[E | E2, Y] = tc.gradedFlatMap(tex)(f)

        def upcast[E2 <: B]: T[E | E2, X] = tc.gradedUpcast[E, E2, X](tex)
      }

      implicit final class toGradedMonadFlattenOps[B, T[_ <: B, _], E1 <: B, E2 <: B, X](
        private val teex: T[E1, T[E2, X]]
      )(implicit tc: GradedMonad[B, T]) {
        def flatten: T[E1 | E2, X] = tc.gradedFlatten[E1, E2, X](teex)
      }
    }
  }

``GradedMonad.gradedPure`` はインスタンスを自動で探してくれる版， ``toGradedMonadOps`` は ``for`` や ``for yield`` 系統の implicit conversion を提供してくれる．その他も幾つか書いたけど，使うのは大体その辺だけになった．例えば，

.. code-block:: scala

  for (
    v1 <- program1
    _  <- program2(v1)
    v2 <- program3(v2)
  ) GradedMonad.gradedPure((v1, v2))

または，

.. code-block:: scala

  for (
    v1 <- program1
    _  <- program2(v1)
    v2 <- program3(v2)
  ) yield (v1, v2)

みたいにコードを書ける．後，こいつが満たさなきゃいけない性質の検査コードも書いておいた:

.. code-block:: scala

  trait GradedMonadLaws[B, T[_ <: B, _]] {
    implicit def T: GradedMonad[B, T]

    def gradedMonadIdentity[E <: B, X](m: T[E, X]): IsEq[T[E, X]] =
      m.upcast <-> m

    def gradedMonadComposition[E1 <: B, E2 <: B, E3 <: B, X](m: T[E1, X]): IsEq[T[E1 | E2 | E3, X]] =
      m.upcast[E2].upcast[E3] <-> m.upcast[E2 | E3]

    def gradedMonadAssociativity[E1 <: B, E2 <: B, E3 <: B, X](m: T[E1, T[E2, T[E3, X]]]): IsEq[T[E1 | E2 | E3, X]] =
      m.flatten.flatten <-> m.map(_.flatten).flatten

    def gradedMonadLeftIdentity[E <: B, X](m: T[E, X]): IsEq[T[E, X]] =
      GradedMonad.gradedPure(m).flatten <-> m

    def gradedMonadRightIdentity[E <: B, X](m: T[E, X]): IsEq[T[E, X]] =
      m.map(GradedMonad.gradedPure(_)).flatten <-> m
  }

``gradedMonadIdentity`` と ``gradedMonadComposition`` は :math:`T` が関手であることを要求してて，後のは lax monoidal functor のコヒーレンス規則になる．

インスタンスを定義する
----------------------

で，具体的にインスタンスも定義してみる．まずは， state から:

.. code-block:: scala

  final case class GradedState[B, S[_ <: B], E <: B, X](val f: GradedStateMapping[B, S, E, X]) {
    def apply[ME <: B](s: S[ME]): (X, S[E | ME]) = f(s)
  }

  trait GradedStateMapping[B, S[_ <: B], E <: B, X] {
    def apply[ME <: B](s: S[ME]): (X, S[E | ME])
  }

  object GradedState {
    def gradedPure[B, S[_ <: B], X](x: X)(
      implicit tc: GradedMonad[B, [E <: B, X] => GradedState[B, S, E, X]]
    ): GradedState[B, S, Nothing, X] = tc.gradedPure(x)

    trait ToGradedStateOps {
      implicit def gradedStateOps[B, S[_ <: B]](
        implicit effectUpcast: EffectUpcast[B, S]
      ): GradedMonad[B, [E <: B, X] => GradedState[B, S, E, X]] = new GradedMonad {
        type T[E <: B, X] = GradedState[B, S, E, X]

        def pfunctor[E <: B]: Functor[[X] => T[E, X]] = new Functor {
          type T[X] = GradedState[B, S, E, X]

          def map[X, Y](m: T[X])(f: X => Y): T[Y] = GradedState(new GradedStateMapping {
            def apply[ME <: B](s: S[ME]): (Y, S[E | ME]) = m(s) match {
              case (x, s2) => (f(x), s2)
            }
          })
        }

        def gradedUpcast[E1 <: B, E2 <: B, X](m: T[E1, X]): T[E1 | E2, X] = GradedState(new GradedStateMapping {
          def apply[ME <: B](s: S[ME]): (X, S[E1 | E2 | ME]) = m(s) match {
            case (x, s2) => (x, effectUpcast.upcast[E1 | ME, E2](s2))
          }
        })

        def gradedPure[X](x: X): T[Nothing, X] = GradedState(new GradedStateMapping {
          def apply[ME <: B](s: S[ME]): (X, S[ME]) = (x, s)
        })

        def gradedFlatten[E1 <: B, E2 <: B, X](m: T[E1, T[E2, X]]): T[E1 | E2, X] = GradedState(new GradedStateMapping {
          def apply[ME <: B](s: S[ME]): (X, S[E1 | E2 | ME]) = m(s) match {
            case (m2, s2) => m2(s2)
          }
        })
      }
    }
  }

もうちょっとうまい定義方法がある気がするけど， Scala 力が足りないのでこうなった．こいつは，

.. math::

  T \epsilon = \int_{\epsilon' \in E} (- \times S(\epsilon \cdot \epsilon'))^{S \epsilon'}

にそのまま対応する．自然数のメモリストアの例も，リテラル型を使えば表すことができて，以下のように作れる:

.. code-block:: scala

  final case class MemoryStore[I <: Int](val f: PartialFunction[I, Int]) {
    def apply(ix: I): Option[Int] = f.lift(ix)
  }

  implicit object MemoryStore extends EffectUpcast[Int, MemoryStore] {
    def empty: MemoryStore[Nothing] = MemoryStore(Map.empty)

    def domainCast[I1 <: Int, I2 <: Int](s: MemoryStore[I1]): MemoryStore[I2] = s match {
      case MemoryStore(f) => MemoryStore({
        case x if f.isDefinedAt(x.asInstanceOf[I1]) => f(x.asInstanceOf[I1])
      })
    }

    def upcast[I1 <: Int, I2 <: Int](s: MemoryStore[I1]): MemoryStore[I1 | I2] = domainCast(s)

    def addValue[I1 <: Int, I2 <: Int](s: MemoryStore[I1])(ix: I2, v: Int): MemoryStore[I1 | I2] = s match {
      case MemoryStore(f) => MemoryStore({
        case x if x.asInstanceOf[I2] == ix => v
        case x if f.isDefinedAt(x.asInstanceOf[I1]) => f(x.asInstanceOf[I1])
      })
    }
  }

  type GradedMemoryState[I <: Int, X] = GradedState[Int, MemoryStore, I, X]

  def getMemoryStore[I <: Int](ix: I): GradedMemoryState[I, Option[Int]] = GradedState(new GradedStateMapping {
    def apply[I2 <: Int](s: MemoryStore[I2]) = (
      MemoryStore.domainCast[I2, I](s)(ix),
      s.upcast[I]
    )
  })

  def putMemoryStore[I <: Int](ix: I, v: Int): GradedMemoryState[I, Unit] = GradedState(new GradedStateMapping {
    def apply[I2 <: Int](s: MemoryStore[I2]) = (
      (),
      MemoryStore.addValue(s)(ix, v)
    )
  })

Int のリテラル型は， ``1 <: Int`` ， ``2 <: Int`` という関係を満たすようになってて，こいつは今までアクセスしたインデックスが， ``1 | 2`` というように型に現れるようになる．他にも，例外モナドを拡張して以下のような graded monad を作れる:

.. code-block:: scala

  enum GradedTry[B, S[_ <: B], E <: B, X] {
    case GradedSuccess(val result: X)
    case GradedFailure(val error: S[E])
  }

  object GradedTry {
    def gradedPure[B, S[_ <: B], X](x: X)(
      implicit tc: GradedMonad[B, [E <: B, X] => GradedTry[B, S, E, X]]
    ): GradedTry[B, S, Nothing, X] = tc.gradedPure(x)

    trait ToGradedTryOps {
      implicit def gradedTryOps[B, S[_ <: B]](
        implicit effectUpcast: EffectUpcast[B, S]
      ): GradedMonad[B, [E <: B, X] => GradedTry[B, S, E, X]] = new GradedMonad {
        type T[E <: B, X] = GradedTry[B, S, E, X]

        def pfunctor[E <: B]: Functor[[X] => T[E, X]] = new Functor {
          type T[X] = GradedTry[B, S, E, X]

          def map[X, Y](m: T[X])(f: X => Y): T[Y] = m match {
            case GradedSuccess(x) => GradedSuccess(f(x))
            case GradedFailure(e) => GradedFailure(e)
          }
        }

        def gradedUpcast[E1 <: B, E2 <: B, X](m: T[E1, X]): T[E1 | E2, X] = m match {
          case GradedSuccess(x) => GradedSuccess(x)
          case GradedFailure(e) => GradedFailure(effectUpcast.upcast[E1, E1 | E2](e))
        }

        def gradedPure[X](x: X): T[Nothing, X] = GradedSuccess(x)

        def gradedFlatten[E1 <: B, E2 <: B, X](m: T[E1, T[E2, X]]): T[E1 | E2, X] = m match {
          case GradedSuccess(m2) => m2 match {
            case GradedSuccess(x) => GradedSuccess(x)
            case GradedFailure(e) => GradedFailure(effectUpcast.upcast[E2, E1 | E2](e))
          }
          case GradedFailure(e) => GradedFailure(effectUpcast.upcast[E1, E1 | E2](e))
        }
      }
    }
  }

こいつは，

.. math::

  T \epsilon = \int_{\epsilon' \in E} S(\epsilon \cdot \epsilon') + (-)

に対応する．なお，現状の Dotty は， higher kinded type に対してのパターンマッチの網羅性検査があまりうまくいかないようで [#dotty-hkt-pattern-check-problem] ，めっちゃ警告が出るけど気にしないでくれ．これを使うと，例外を複数種類投げる計算を管理することができて，

.. code-block:: scala

  sealed abstract class CustomException
  final case class Exception1() extends CustomException
  final case class Exception2() extends CustomException
  final case class Exception3() extends CustomException

  final case class Exc[E <: CustomException](val exc: E)

  implicit object Exc extends EffectUpcast[CustomException, Exc] {
    def upcast[E1 <: CustomException, E2 <: CustomException](e: Exc[E1]): Exc[E1 | E2] = e match {
      case Exc(e) => Exc(e)
    }
  }

  type GradedExcTry[E <: CustomException, X] = GradedTry[CustomException, Exc, E, X]

  def fromEither[E <: CustomException, X](r: Either[E, X]): GradedExcTry[E, X] = r match {
    case Left(e)  => GradedTry.GradedFailure(Exc(e))
    case Right(v) => GradedTry.GradedSuccess(v)
  }

みたいな物を用意してやると， ``Exception1`` を投げるプログラムと ``Exception2`` を投げるプログラムを合成した時，ちゃんと ``Exception1 | Exception2`` の例外を投げるプログラムにできる．

まとめ
------

まだやってないんだけど，モナドの時と同じようにして， freer な graded monad を考えることもできそう．こいつはうまく作れればハンドルできたりもして， Dotty で algebraic effect を再現できたりしないかなと思ってる．まあ，まだ思ってるだけだけど．時間があったらその辺も試してみたい．

Dotty かなり気軽に触れて良さそう． Scala 力もちょっと上がった気がする． Dotty だと union type があるから結構実装できたけど， Haskell とかだとちょっと厳しそう？ あまり深く考えていない．また時間があったら試してみようと思う．

.. [#dotty-hkt-pattern-check-problem] https://github.com/lampepfl/dotty/issues/6088
