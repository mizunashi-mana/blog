Kotlin の coroutine の仕組み
============================

:tags: Kotlin, Java, Android, 並行処理, 継続
:category: プログラミング言語

Kotlin には coroutine と呼ばれるシステムが搭載されている．1.2 までは experimental だったが，1.3 では stable になり，現在は一般的に使われている．ところで，coroutine の使用方法はいろんな人が書いてるんだけど，中身についての情報となるとかなり見つけるのが大変になるので，調べたことをまとめておこうと思う．

Kotlin の coroutine
-------------------

Kotlin では，主に non-blocking な処理を実現するための仕組みとして，coroutine システムが搭載されている．ただ，これは Java や Scala の ``Future`` とはまた違った使い勝手になっている．

言語機能としての coroutine
--------------------------

まずは，coroutine システムの言語機能について見ていく．と言ってもそれほど複雑なことはしてなくて，結構素直な delimited continuation の実装になっている．内容的には，

  Kotlin Coroutine Proposal: https://github.com/Kotlin/KEEP/blob/master/proposals/coroutines.md

の劣化コピーになるので，原文当たってくれた方が良いかもね．用語として，

コルーチン (coroutine)
  **中断されるかもしれない計算 (suspendable computation)** の実体 [#notice-coroutine-is-an-instance]_ のこと．実行可能なコード断片を持っていて，作成された後，開始し，どこかで中断再開を繰り返しながら，最終的に例外か値を結果として完了する．

中断関数 (suspending function)
  ``suspend`` キーワードがついた関数．中断されるかもしれない計算を定義する．ラムダで定義されたものを **中断ラムダ (suspending lambda)**，この関数の型を **中断関数型 (suspending function type)** と呼ぶ．

中断地点 (suspension point)
  コルーチンにおける中断可能な点．主に中断関数上で他の中断関数を呼んでいる場所が中断地点になる [#actual-suspension-point]_．イメージ的には，今中断している箇所以降の計算を表す．

継続 (continuation)
  中断されているコルーチンの中断地点を表す状態のこと．例えば，

  .. code-block:: kotlin

    suspend fun doSomething1()
    suspend fun doSomething2()
    suspend fun doSomething3()

    suspend fun doSomething() {
      doSomething1()
      doSomething2()
      doSomething3()
    }

  という中断関数 ``doSomething`` があり，そこから作られたコルーチンが ``doSomething2()`` の呼び出し部分で中断しているなら，このコルーチンの継続は ``doSomething2()`` の地点になる．

コルーチン生成器 (coroutine builder)
  中断関数を引数に取り，そいつを元にコルーチンを作る関数のこと．こいつに関してはかなり抽象的に定義されているが，これは単に通称だから．プリミティブに提供されている生成器もあるが，ほとんどはライブラリレベルで提供されており，ユーザも定義できる．本当に，コルーチンを作るやつはなんでもコルーチン生成器と呼称していく．

というのを使っていく．さて，まず結論から言っておくと，

.. code-block:: kotlin

  suspend fun doSomething1()
  suspend fun doSomething2()
  suspend fun doSomething3()

  suspend fun doSomething() {
    doSomething1()
    doSomething2()
    doSomething3()
  }

というプログラムは，気分的には以下のように実体が吐かれる:

.. code-block:: kotlin

  interface Result<out T> {
    fun getOrThrow(): T
  }

  interface Continuation<in T> {
    val context: CoroutineContext
    fun resumeWith(result: Result<T>)
  }

  fun <T> Continuation.resume<T>(value: T) {
    resumeWith(object : Result<T> {
      override fun getOrThrow() = value
    })
  }

  fun <T> Continuation.resumeWithException(exception: Throwable) {
    resumeWith(object : Result<T> {
      override fun getOrThrow() {
        throw exception
      }
    })
  }

  fun doSomething1(cont: Continuation<Unit>)
  fun doSomething2(cont: Continuation<Unit>)
  fun doSomething3(cont: Continuation<Unit>)

  fun doSomething(cont: Continuation<Unit>) {
    doSomething1(object : Continuation {
      override fun resumeWith(result: Result<Unit>) {
        try {
          result.getOrThrow

          doSomething2(object : Continuation {
            override fun resumeWith(result: Result<Unit>) {
              try {
                result.getOrThrow

                doSomething3(cont)
              } catch (e: Throwable) {
                cont.resumeWithException(e)
              }
          })
        } catch (e: Throwable) {
          cont.resumeWithException(e)
        }
      }
    })
  }

``CoroutineContext`` の中には，それぞれのコルーチン特有の情報が入っており，例えば非同期プログラミングによるコルーチンではジョブの管理情報などが入っていて，その管理機構を提供するライブラリは ``CoroutineContext`` にアクセスする API などを提供するようになる．で，専用のコルーチン生成器が初期のコンテキストを生成する感じになる．ま，上のコードはあくまでイメージで，実際には色んなプリミティブな API が提供されていて，その API がいい感じにコード生成されるので，もうちょっと複雑かつ実行機に優しい感じのコードが生成されるけど，本質的には上の感じのことが起こるだけと思ってもらって問題ない．さて，ではその詳細を見ていく．

まず，Kotlin では上記のコードでも使用したように，継続を表すインターフェースが用意されている:

.. code-block:: kotlin

  interface Continuation<T> {
    val context: CoroutineContext<T>
    fun resumeWith(result: Result<T>)
  }

``CoroutineContext`` は後ほど説明するのでしばらくは無視しておいていい．``resumeWith`` は中断したコルーチンの継続に対して，そこまでの結果を入れることでコルーチンを再開するメソッドになっている．``Result<T>`` は成功か失敗の結果を表す抽象データ型になっていて，成功の場合 ``T`` 型の値，失敗の場合 ``Throwable`` な値が受け取れるようになっている．なお，標準では以下の 2 つの拡張関数も提供されている:

.. code-block:: kotlin

  fun <T> Continuation<T>.resume(value: T)
  fun <T> Continuation<T>.resumeWithException(exception: Throwable)

見ての通り，``resumeWith`` を成功の結果で呼び出すのが ``resume``，失敗の結果で呼び出すのが ``resumeWithException`` になる．中断関数上で継続を切り出すには，以下の API を呼び出す:

.. code-block:: kotlin

  suspend fun <T> suspendCoroutine(block: (Continuation<T>) -> Unit): T

この中断関数は，この関数が呼ばれた中断地点以降の継続を切り出し，もらってきたブロックに渡す．例えば，

.. code-block:: kotlin

  suspend fun doSomething() {
    doSomething1()

    val i = suspendCoroutine { cont ->
      cont.resume(0)
    }
    doSomething2(i)
  }

という中断関数があった時，``cont`` が指すものは，``val i = _; doSomething2(i)`` というようなプログラム片で，この ``_`` の部分に ``resume`` に渡されたものを入れると思うといい．中断関数は，最初に挙げた通り，全て暗黙的に継続が付与されるようなものだと思えばいいので，``suspendCoroutine`` は実際には

.. code-block:: kotlin

  fun <T> suspendCoroutine(block: (Continuation<T>) -> Unit, cont: Continuation<T>): T

という型を持ち，ブロックに単に受けとった継続を渡すものだというイメージだ．

なお，切り出された継続は one-shot で 1 回しか使えず，複数回使うと ``IllegalStateException`` という例外が返ってくる．この例外の名前が何でこうなってるかは，想像つく人は想像つくと思うけど，中断関数の実装に由来している．中断関数は，最初に挙げた通り，気分的には継続を表すコールバックを次々と渡すイメージなんだけど，実際には状態遷移で実装されている [#notice-tail-call-optimization]_．例えば，

.. code-block:: kotlin

  suspend fun doSomething1()
  suspend fun doSomething2()
  suspend fun doSomething3()

  suspend fun doSomething() {
    doSomething1()
    doSomething2()
    doSomething3()
  }

は，次のように変換される:

.. code-block:: kotlin

  fun doSomething(completion: Continuation<Unit>): Any? = {
    val cont: Continuation<Unit> =
      if (
        completion is Cont_doSomething &&
        (completion.label and Integer.MIN_VALUE) != 0
      ) {
        completion.label = completion.label - Integer.MIN_VALUE
        completion
      } else {
        Cont_doSomething(completion)
      }

    var result = cont.result

    if (cont.label == 0) goto L0
    if (cont.label == 1) goto L1
    if (cont.label == 2) goto L2
    if (cont.label == 3) goto END
    else throw IllegalStateException()

  L0:
    result.throwOnFailure
    cont.label = 1
    result = doSomething1(cont)
    if (result == COROUTINE_SUSPENDED) return result
  L1:
    result.throwOnFailure
    cont.label = 2
    result = doSomething2(cont)
    if (result == COROUTINE_SUSPENDED) return result
  L2:
    result.throwOnFailure
    cont.label = 3
    result = doSomething3(cont)
    if (result == COROUTINE_SUSPENDED) return result
  END:
    result.throwOnFailure
    return result
  }

  class Fun_doSomething(
    completion: Continucation<*>
  ) : ContinuationImpl(completion) {
    public var result: Any = null
    public var label: Int = 0

    override fun invokeSuspend(result: Any) {
      this.result = result
      this.label = this.label or Integer.MIN_VALUE
      return doSomething(this)
    }
  }

``goto`` とラベルは，特別な機能だと思ってくれ．``doSomething1`` などは中で中断地点が無ければ ``resume`` を呼ばずに単純にその値を返してきて，その値は ``COROUTINE_SUSPENDED`` ではないので次のラベルにいく．中で ``resume`` がある場合は，返り値は ``COROUTINE_SUSPENDED`` という値になりそこでコルーチンが中断するという感じだ．ちょっと色々謎の部分もあると思うが，とりあえずやってることは，``L1 -> L2 -> L3 -> END`` というように中断地点が移動して行く状態遷移になっていて，``resume`` が2 回呼び出されたりするとその状態遷移が壊れて変な状態に移ってしまうということだ．結果，``IllegalStateException`` が吐かれる．

coroutine システムの実際の動き
------------------------------

ここまでがイメージをとらえる説明なわけだけど，実際には中はもうちょっと複雑になっている．ここからは，もうちょっとそれぞれを深掘りしていく．主に，上の中断関数から変換されたコードが実際にはどういう動きをするのかを見ていく．

上のコードは，どこの中断地点でも ``COROUTINE_SUSPENDED`` が返ってこない場合は，そのまま単に流れていくだけで通常の関数と同じ動作をする．では，``COROUTINE_SUPENDED`` が返ってくる場合はどういう動作をするんだろうか？ そもそも，``COROUTINE_SUSPENDED`` はどういう状況で返ってくるんだろうか？

コルーチン生成器
----------------

構造的並行性
------------

まとめ
------

.. [#notice-coroutine-is-an-instance] 計算自体を指すんじゃなくて，実際に動くタスクみたいなもののことを指す．ここでいうタスクは，一般用語としてのタスクで，要は仕事内容のことではなくお仕事そのものを指すような概念が coroutine，仕事内容が suspendable computation だと思ってもらえればいい．なお，タスクも別のとこで technical term として導入するので，そこと混同しないようにして欲しい．
.. [#actual-suspension-point] 実際に中断される場所は，中断用のプリミティブ操作が実行されてる場所になる．
.. [#notice-tail-call-optimization] なお，単純な末尾呼び出ししか出てこないものに関しては，素直に CPS 変換するっぽい．
