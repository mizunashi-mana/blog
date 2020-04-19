gRPC クライアントをAndroid アプリで書く
=======================================

:tags: Kotlin, Java, Gradle, Android, gRPC
:category: フレームワーク

単純に gRPC クライアントを Android アプリで書いて，mock サーバを Kotlin で書くだけなのにすごく苦労したので，備忘録的に書いとく．なお，最終的に出来上がったものは，https://github.com/mizunashi-mana/grpc-android-example に上げてある．

Android の Gradle プロジェクトの基本
------------------------------------

まず，Android Studio で新規プロジェクトを作る．基本はここから大きく変更しなくて良い．Gradle のバージョンが古かったりするが，それは android 向けの gradle プラグインが最新に対応していないから．具体的な対応状況は，https://developer.android.com/studio/releases/gradle-plugin?hl=ja#updating-gradle に書いてある．現状の作成段階では，

* android gradle plugin: 3.6.3
* 対応してる gradle の最新: 5.6.4

みたいな感じだった．で，Android Studio が生成する ``build.gradle`` は使う gradle のバージョンが明示されていないので，書いとくといい [#notice-gradle-wrapper]_:

.. code-block:: groovy

  wrapper {
    gradleVersion = '5.6.4'
  }

なお，このバージョンでうまく動く JDK のバージョンは限られていて，基本 JDK 8 を使うのが安定．``direnv`` とかで ``JAVA_HOME`` を JDK 8 のものにしとくと良い．

Android の Gradle 構成は，ルートにある ``build.gradle`` がプロジェクト全体のもの，``app/build.gradle`` が Android アプリモジュール用と言うようになっている．プロジェクト全体で共通の設定はルートの ``build.gradle`` に，モジュール専用のものは ``app/build.gradle`` に書いとくと良い．

gRPC のモックサーバを Kotlin で書く
-----------------------------------

まずはモックサーバの方を足してく．まずは全体設定から．共通で使うバージョン情報などを，ルートの ``build.gradle`` に埋め込む:

.. code-block:: groovy

  buildscript {
    ext.kotlin_version = '1.3.61'
    ext.coroutines_version = '1.3.3'
    ext.protobuf_version = '3.11.1'
    ext.grpc_version = '1.28.1'
    ext.grpc_kotlin_version = '0.1.1'

    ...
  }

Kotlin 用の gRPC ライブラリ，``grpc-kotlin`` は，coroutine flow を使っていい感じに streaming rpc の処理を書けるようになってるので，その為に coroutine api のバージョン情報を足している．後は使用する

* ``protobuf`` のバージョン
* ``grpc-java`` のバージョン
* ``grpc-kotlin`` のバージョン

を指定している．これらは，モックサーバと Android アプリで共通のバージョンを使う．後は，``proto`` ファイルをサーバとアプリ共通で使う為，ルートに ``proto`` ディレクトリを作りその中に置くようにする．そして，そのパスも共通設定に入れておく:

.. code-block:: groovy

  buildscript {
    ...

    ext.proto_dir = file('proto')

    ...
  }

後，``protobuf`` の gradle プラグインを使う為，classpath を追加しておく:

.. code-block:: groovy

  buildscript {
    ...

    dependencies {
        ...
        classpath 'com.google.protobuf:protobuf-gradle-plugin:0.8.12'
    }
  }

次に，モックサーバ用に gradle のモジュールをさらに生やす．やることとしては，

1. ``mock`` ディレクトリを作る
2. ``mock/build.gradle`` を書く
3. ``mock/.gitignore`` を書く
4. ``mock`` モジュールのメインクラスを作る

みたいな感じ．``build.gradle`` は，まず必要なプラグインを書いていく:

.. code-block:: groovy

  apply plugin: 'application'
  apply plugin: 'kotlin'
  apply plugin: 'idea'
  apply plugin: 'com.google.protobuf'

``application`` プラグインは，``run`` とか生やしたり，配布用の実行ファイルアーカイブ作ってくれたりするやつ．``kotlin`` と ``com.google.protobuf`` は名前の通り．``idea`` プラグインは最初何のために必要なのか分かってなかったんだけど，こいつがないと ``protobuf`` プラグインが生成した kotlin ファイルを何故か gradle が認識してくれない．一応表面上は JetBrains 製エディタと連携するためのプラグインということになっているが，とりあえずこいつ入れないと ``grpc-kotlin`` が機能しないので入れてる．意味不明なので，誰か原因と解決方法知ってる人いたら教えてくれって感じ．

後は，メインクラスの指定と ``proto`` ディレクトリの指定をやっていく:

.. code-block:: groovy

  mainClassName = 'com.example.myapplication.mock.MainKt'

  sourceSets {
      main {
          proto {
              srcDir proto_dir
          }
      }
  }

メインクラスは，オブジェクト名 + Kt にする．それから，依存ライブラリを指定していく:

.. code-block:: groovy

  dependencies {
      compileOnly 'javax.annotation:javax.annotation-api:1.2'

      implementation "io.grpc:grpc-kotlin-stub:${grpc_kotlin_version}"

      implementation 'org.jetbrains.kotlin:kotlin-stdlib-jdk8:1.3.61'
      implementation "org.jetbrains.kotlinx:kotlinx-coroutines-core:${coroutines_version}"

      implementation "com.google.protobuf:protobuf-java:${protobuf_version}"
      implementation "com.google.protobuf:protobuf-java-util:${protobuf_version}"
      implementation "io.grpc:grpc-netty-shaded:${grpc_version}"
      implementation "io.grpc:grpc-protobuf:${grpc_version}"
      implementation "io.grpc:grpc-stub:${grpc_version}"
  }

``javax.annotation-api`` とその他の ``grpc`` 関連のは ``proto`` から生成される Kotlin ファイルに必要．後は，まあ必要なもの一式って感じ．それから，``protobuf`` の出力設定を書く:

.. code-block:: groovy

  protobuf {
      protoc {
          artifact = "com.google.protobuf:protoc:${protobuf_version}"
      }

      plugins {
          grpc {
              artifact = "io.grpc:protoc-gen-grpc-java:${grpc_version}"
          }

          grpckt {
              artifact = "io.grpc:protoc-gen-grpc-kotlin:${grpc_kotlin_version}"
          }
      }

      generateProtoTasks {
          all().each { task ->
              task.plugins {
                  grpc { }
                  grpckt { }
              }
          }
      }
  }

``grpc-kotlin`` の出力ファイルは，基本 ``grpc-java`` の生成ファイルを元にしているっぽいので，どっちの出力オプションも加えておく．これで，``proto`` ファイルからいい感じの Kotlin ファイルが出来上がるようになる．

``mock/.gitignore`` は，``/build`` だけ書いとけばおk．

後は，``mock`` モジュールのメインクラスを作っておく．とりあえず，

.. code-block:: kotlin

  package com.example.myapplication.mock

  fun main() {
    println("Hello, World!")
  }

と書いて動作確認して見る::

  $ ./gradlew mock:run
  ...
  Hello, World!

次に，``proto`` ファイルを作る．``proto/hello.proto`` に，

.. code-block:: kotlin

  syntax = "proto3";

  package com.example.myapplication.proto;

  service Service {
      rpc Connect (stream Post) returns (stream Post) {}
  }

  message Post {
      string message = 1;
  }

という感じで，双方向にメッセージを垂れ流すインターフェースを定義しておく．なお，オプションで出力する Java 用のパッケージ名などはいい感じに `制御できる <https://developers.google.com/protocol-buffers/docs/proto3#options>`__．今回は，両方 Kotlin で特にサーバとクライアントでパッケージ名別にする必要とかないので，``package`` 部分をそのまま使ってもらう．後は，モックサーバ本体を書く．さっきのメインクラスを

.. code-block:: kotlin

  /*
   * Copyright 2020 gRPC authors.
   *
   * Licensed under the Apache License, Version 2.0 (the "License");
   * you may not use this file except in compliance with the License.
   * You may obtain a copy of the License at
   *
   * http://www.apache.org/licenses/LICENSE-2.0
   *
   * Unless required by applicable law or agreed to in writing, software
   * distributed under the License is distributed on an "AS IS" BASIS,
   * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   * See the License for the specific language governing permissions and
   * limitations under the License.
   */

  package com.example.myapplication.mock

  import com.example.myapplication.proto.Hello
  import com.example.myapplication.proto.ServiceGrpcKt
  import io.grpc.Server
  import io.grpc.ServerBuilder
  import kotlinx.coroutines.flow.Flow
  import kotlinx.coroutines.flow.collect
  import kotlinx.coroutines.flow.flow

  class HelloServer constructor(
      private val port: Int
  ) {
      val server: Server = ServerBuilder
          .forPort(port)
          .addService(HelloService())
          .build()

      fun start() {
          server.start()
          println("Server started, listening on $port")
          Runtime.getRuntime().addShutdownHook(
              Thread {
                  println("*** shutting down gRPC server since JVM is shutting down")
                  this@HelloServer.stop()
                  println("*** server shut down")
              }
          )
      }

      private fun stop() {
          server.shutdown()
      }

      fun blockUntilShutdown() {
          server.awaitTermination()
      }

      private class HelloService: ServiceGrpcKt.ServiceCoroutineImplBase() {
          override fun connect(requests: Flow<Hello.Post>): Flow<Hello.Post> = flow {
              requests.collect { request ->
                  emit(request)
              }
          }
      }
  }

  fun main() {
      val port = 5000
      val server = HelloServer(port)
      server.start()
      server.blockUntilShutdown()
  }

`公式のサンプル <https://github.com/grpc/grpc-kotlin/tree/v0.1.1/examples>`_ ちょっと書き換えただけのやつ．これを走らせると，

::

  $ ./gradlew mock:run
  ...
  Server started, listening on 5000

って感じで待機して，5000 番に gRPC サーバが立つ．こいつ自体は単なるエコーサーバで，送ってきたやつそのまま送り返すだけ．

Android アプリで gRPC クライアントを作る
----------------------------------------

Android アプリ側も同じく，``build.gradle`` を調整する．まず ``protobuf`` プラグインを足す:

.. code-block:: groovy

  apply plugin: 'com.google.protobuf'

それから，``proto`` ディレクトリを設定する:

.. code-block:: groovy

  android {
    ...

    sourceSets {
        main {
            proto {
                srcDir proto_dir
            }
        }
    }

    ...
  }

後は，依存ライブラリを足す:

.. code-block:: groovy

  dependencies {
    ...
    compileOnly 'javax.annotation:javax.annotation-api:1.2'
    implementation "io.grpc:grpc-okhttp:${grpc_version}"
    implementation "io.grpc:grpc-protobuf-lite:${grpc_version}"
    implementation "io.grpc:grpc-stub:${grpc_version}"
  }

Android 側は ``protobuf-lite`` というのを使う．これは軽量版になってて，通常の JVM に吐き出すよりいい感じに Android 用に最適化してコードサイズ減らしたクラスを生成してくれるらしい．で，それ用に ``protobuf`` の設定も調整する:

.. code-block:: groovy

  protobuf {
      protoc {
          artifact = "com.google.protobuf:protoc:${protobuf_version}"
      }

      plugins {
          grpc {
              artifact = "io.grpc:protoc-gen-grpc-java:${grpc_version}"
          }
      }

      generateProtoTasks {
          all().each { task ->
              task.builtins {
                  java {
                      option 'lite'
                  }
              }
              task.plugins {
                  grpc {
                      option 'lite'
                  }
              }
          }
      }
  }

それから，Android のマニフェストでネットワーク通信の権限が得られるようにしておく:

.. code-block:: xml

  <?xml version="1.0" encoding="utf-8"?>
  <manifest xmlns:android="http://schemas.android.com/apk/res/android"
      ...>

      <uses-permission android:name="android.permission.INTERNET" />

      ...
  </manifest>

これがないと，``socket failed: EPERM (Operation not permitted)`` と言われてクライアントが立ち上がらない．後は，適当にクライアント書くだけ．アプリのデザインは最初にあげたリポジトリみれくれって感じ．メインアクティビティのコードだけ載せとく:

.. code-block:: kotlin

  package com.example.myapplication

  import android.os.Bundle
  import android.os.Handler
  import android.util.Log
  import androidx.appcompat.app.AppCompatActivity
  import androidx.databinding.DataBindingUtil
  import com.example.myapplication.proto.Hello
  import com.example.myapplication.proto.ServiceGrpc
  import io.grpc.ManagedChannelBuilder
  import io.grpc.stub.StreamObserver
  import com.example.myapplication.databinding.ActivityMainBinding

  class MainActivity : AppCompatActivity() {

      private lateinit var handler: Handler
      private lateinit var stream: StreamObserver<Hello.Post>
      private lateinit var binding: ActivityMainBinding

      private var i: Int = 0

      override fun onCreate(savedInstanceState: Bundle?) {
          super.onCreate(savedInstanceState)

          handler = Handler()
          binding = DataBindingUtil.setContentView(this, R.layout.activity_main)
          stream = startgRPCClient(handler, binding)

          binding.button.setOnClickListener {
              stream.onNext(
                  Hello.Post.newBuilder()
                      .setMessage("button clicked: $i")
                      .build()
              )
              i++
          }
      }

      companion object {
          private fun startgRPCClient(
              mainHandler: Handler,
              binding: ActivityMainBinding
          ): StreamObserver<Hello.Post> {
              val channel = ManagedChannelBuilder
                  .forAddress("10.0.2.2", 5000)
                  .usePlaintext()
                  .build()
              val stub = ServiceGrpc.newStub(channel)

              return stub.connect(object : StreamObserver<Hello.Post> {
                  override fun onNext(post: Hello.Post) {
                      mainHandler.post {
                          binding.textView.text = post.message
                      }
                  }

                  override fun onError(t: Throwable) {
                      Log.e("app", "gRPC error", t)
                  }

                  override fun onCompleted() {
                      Log.i("app", "gRPC connection closed")
                  }
              })
          }
      }
  }

ほぼ見ての通りですって感じだけど，画面にテキストビューとボタンが設置してあって，ボタンが押されるたびその回数をカウントして，``button clicked: [回数]`` って感じのメッセージをサーバに投げてる．で，サーバから来た最新のメッセージがテキストビューに表示されるようになっていて，側から見るとボタン押すたびテキストビューの数値が更新されていく．

一点注意点として，テキストビューの更新の際，直接更新しないで ``Handler`` というものを使っている．これは，Android の仕様として，ビューの更新はビューを作ったスレッド，今回はメインスレッドでやる必要があるらしく，``StreamObserver`` 内でやると別スレッドでの更新になってしまうのでエラーになるため．そうした時は，``Only the original thread that created a view hierarchy can touch its views.`` みたいな怒られが発生する．なので，一回メインスレッドに移譲してやる必要がある．この移譲のための ``Handler`` をメインスレッドで作る (またはメインスレッドを指定して作る) とかして，やってるのが上のやつ．後は良いんじゃないんですかね．

まとめ
------

というわけで，モックサーバと Android アプリで gRPC 通信するやつ，全 Kotlin で書いてみた．たったこんだけ書くのに，結構ハマりポイントがあって辛かった．でも，なんとなく gradle のお作法が分かった気がする．後，Kotlin はいいぞ．

.. [#notice-gradle-wrapper] なお，gradle wrapper は生成されていて，``gradle-wrapper.properties`` にはバージョンが記載されているので，書かなくてもいい．wrapper のバージョンを上げる時とか用に書いとくといいと言う感じ．
