KaTeX を静的にコンパイルする
============================

:date: 2019-04-28 18:00
:tags: KaTeX, Pelican, ブログ, Python, Node.js, JavaScript
:category: 運用

ふと， KaTeX ってサーバサイドレンダリングできないかと思って試してみたら，普通に楽にできそうだったので，ブログでやってみることにした．

KaTeX をサーバサイドレンダリングする
------------------------------------

特に難しいことはなくて， KaTeX は Node.js 上で動くっぽく，以下のようにすれば HTML を生成できるっぽい::

  $ node --eval 'const KaTeX = require("katex"); console.log(KaTeX.renderToString("x"));'
  <span class="katex"><span class="katex-mathml"><math><semantics><mrow><mi>x</mi></mrow><annotation encoding="application/x-tex">x</annotation></semantics></math></span><span class="katex-html" aria-hidden="true"><span class="base"><span class="strut" style="height:0.43056em;vertical-align:0em;"></span><span class="mord mathdefault">x</span></span></span></span>

これを利用して， Pelican のプラグインから Node.js で KaTeX のレンダリングを行うようにする．まず， JavaScript のコードで，次のようなものを書いた:

.. code-block:: javascript

  #!/usr/bin/env node

  const KaTeX = require("katex");

  function katexRenderingPipe(instream, outstream) {
    let chunkrest = "";

    function onData(chunk) {
      const chunks = chunk.toString("utf8").split("\t\n");
      chunks[0] = chunkrest + chunks[0];

      let l = chunks.length - 1
      for (i = 0; i < l; i++) {
        const str = chunks[i].replace(/\t /g, "\t");
        if (typeof str !== "string") {
          throw new Error("not string stream");
        }

        outstream.write(KaTeX.renderToString(str).replace(/\t/g, "\t "));
        outstream.write("\t\n");
      }
      chunkrest = chunks[l];
    }

    instream.on("data", onData);
    instream.on("end", () => {
      outstream.end('');
    });
  }

  katexRenderingPipe(process.stdin, process.stdout);

このプログラムは，区切り文字で区切って送られてくる数式文字列を受け取って，それを ``KaTeX`` に渡し，レンダリングされた文字列を返すプログラムになる．データのシリアライズ形式としてめっちゃ単純なものを使っていて，ほぼ文字列そのままでタブ文字はその後に空白を入れる．後はその文字列をタブ文字改行で区切って送るだけ．送った先ではタブ文字空白をタブ文字に変換する．こうすると，区切り文字が一定に決まる． HTTP でも過去には一部のブラウザで似たようなことが行われていた気がする．タブ文字は，送る数式にも返ってくる HTML にも入ってないであろうと想定して，デバッグの時に視覚的に見分けられる文字として選んでる．後は， Pelican 側から，上のスクリプトを立ち上げて随時数式文字列を送り，返ってきた HTML で数式環境を置き換えるだけ．ただそれだけなんだが， Python 初心者なので色々手こずった．なので，以降主に Python についての内容になる．

asyncio を使う
--------------

Node.js に合わせて Python も非同期 I/O を使おうと思って， Python 標準の ``asyncio`` モジュールを使ってみることにした． Python には， 3.5 から async/await 構文が入っている．感覚的には， JavaScript の async/await と変わらなくて， ::

  async def f(x):
    return await g(x)

は，以下のと大体同じ::

  import asyncio

  @asyncio.coroutine
  def f(x):
    return yield from g(x)

``@asyncio.coroutine`` デコレータは将来消されるらしいけど [#asyncio-coroutine-decolator-deprecated]_，イメージ的にはジェネレータの各ステップを適宜 Future に包むながら繋げていく感じ．で，そのデコレータを使わないで，同様のことを簡単に書けるのが async / await 構文で，デコレータに包まれたジェネレータの定義が ``async def`` ， ``yield from`` が ``await`` な感じ．まあ，実は僕もそこまで深く理解してないんだけど．

Node.js との違いは，イベントループに対する明示的な操作がかなりできることっぽい．イベントループ止めたりもできる．この辺よく分かってないけど，とりあえず ::

  asyncio.run(f("arg"))

とかしとけば，

1. イベントループをカレントスレッドに作成する (既にあればエラーになる) ．
2. 返ってきたコルーチンオブジェクトを，作成したイベントループで処理する．
3. 処理し終えたら，イベントループを破棄して，結果に応じて値または例外を出す．

みたいなことをしてくれるっぽい．他にも色々 API があるんだけど，フォークに対応してなかったり [#fork-asyncio-issue]_，微妙に使い方分からなかったり．なので，適当に ``run`` 使うことにした．

``asyncio`` モジュールでは，この async/await に対応した非同期 I/O API も提供している．今回は， Node.js のスクリプトを立ち上げるため，サブプロセス関係の API を使う． ``asyncio`` モジュールのサブプロセス関係の API は，基本的に ``subprocess`` モジュールと扱い方は変わらなくって，コルーチンオブジェクトを返すようになってるだけ．以下のように使う:

.. code-block:: python

  import sys
  import asyncio

  async def ls1():
    proc = await asyncio.create_subprocess_exec(
      "ls",
      "-1",
      stdin=None,
      stdout=asyncio.subprocess.PIPE,
      )

    while True:
      line = await proc.stdout.readline()
      if len(line) == 0:
        break

      sys.stdout.buffer.write(line)

    await proc.wait()

  asyncio.run(ls1())

ところで，対象が ``ls`` コマンドならいいんだけど，今呼び出したいプログラムは EOF を送らないと終了しないようになっているので，呼び出し側で例外が発生すると，そのままプロセスが閉じないで色々あれなことになる．なので，プロセスのリソース管理をして，例外が発生したら呼び出してるプロセスに ``SIGTERM`` を送るようにしたい．これには， with 構文の async 版， async with が使える． async with は ``__aenter__`` / ``__aexit__`` というプロトコルメソッドを使う．サブプロセスの管理は，以下のように書ける:

.. code-block:: python

  STREAM_CHUNK_LIMIT = 2 ** 16

  class KaTeXRendererManager:
    async def __aenter__(self):
      self.proc = await asyncio.create_subprocess_exec(
        'node',
        os.path.join(os.path.dirname(os.path.abspath(__file__)), 'katex_render.js'),
        stdin=asyncio.subprocess.PIPE,
        stdout=asyncio.subprocess.PIPE,
        limit=STREAM_CHUNK_LIMIT,
        )

      return self.proc

    async def __aexit__(self, exc_type, exc, tb):
      if exc is not None:
        self.proc.terminate()
        return

      await self.proc.wait()

  async def render_math_contents(contents):
    async with KaTeXRendererManager() as proc:
      async def readuntil_sep():
        chunk = b''
        while True:
          try:
            chunk = chunk + await proc.stdout.readuntil(b'\t\n')
          except asyncio.LimitOverrunError:
            chunk = chunk + await proc.stdout.read(STREAM_CHUNK_LIMIT - 1024)
            continue
          else:
            break

        return chunk.replace(b'\t ', b'\t')

      for content in contents:
        proc.stdin.write(content.replace(b'\t', b'\t ').encode())
        proc.stdin.write(b'\t\n')

        print(await readuntil_sep())

      proc.stdin.write_eof()

  asyncio.run(render_math_contents(['f(x) = 2^x', 'n \\in \\mathbb{N}']))

``async with`` が呼ばれた時に， ``__aenter__`` で返されたものが束縛され，処理が終了するか例外が出ると ``__aexit__`` が呼ばれる．なお， ``__aexit__`` は，正常終了すると引数に ``None`` が入れられて呼ばれる．なお，サブプロセスから読み取るチャンクの最大サイズはデフォルトで， 64KB (= :math:`2 ^ {16}` B) が指定されてる．これを超えるものを読み取ろうとすると， ``LimitOverrunError`` 例外が発生する::

  ValueError: Separator is not found, and chunk exceed the limit

これを解決するため，例外が発生するとまずちょっと読み取って，それから再度挑戦するみたいなことをしてる．ここまでくれば，後は適当に BeautifulSoup を使って数式 DOM 部分を置換していくプラグインを書くだけだった．

Python のデバッグをする
-----------------------

プラグインを書く際，結構色々バグってデバッグをたくさんした．特に， Pelican だとエラーはメッセージしか表示されなくて，トレースバックが表示されなくて，色々難航したので，トレースバックを表示する方法を色々調べた．結論としては，以下のようにすればトレースバックを表示できるっぽい:

.. code-block:: python

  import traceback

  try:
    ...
  except Exception as e:
    for line in traceback.format_exception(None, e, e.__traceback__):
      print(line, end='')

    raise e

Python 3.5 から ``traceback.format_exception`` の一番目の引数は勝手に検知してくれるようになったので，特に指定しなくて大丈夫みたい．推測してくれない場合，エラーの型を書く必要がある．後，プロファイルが取りたければ次のようにする:

.. code-block:: python

  import pstats
  import cProfile

  pr = cProfile.Profile()

  pr.enable()
  ...
  pr.disable()

  """
  ファイル出力の場合:
  pstats.Stats(pr).dump_stats('profile.stats')

  pstats.Stats('profile.stats')
  """

  # 累計時間でソートして，上位 10 件表示
  pstats.Stats(pr).sort_stats(pstats.SortKey.CUMULATIVE).print_stats(10)

なんかまあ，そんな感じ．

まとめ
------

サーバサイドレンダリングで， JavaScript をブラウザで動かさずに数式を見れるようにした．でも，結構ビルドが遅くなっちゃった．

なんか色々調整してみたけど，大体文書 18 個，ページ 3 個で 8 秒ぐらいかかる． KaTeX のビルドをしなければ 3 秒ぐらいで済む．これ何とかしたいんだけど， KaTeX は Node.js 上でしか動かないし， Pelican は Python が必要だし，プラグインの制約も色々あるし，うーんという感じ．多分サブプロセスの立ち上げと， Node.js と Python 間の通信が一番コストになってる．通信は gzip 圧縮でもしてみようかと思ったけど，めんどいのでやめた．まあ，色々対策を考えてみるかという感じ．

.. [#asyncio-coroutine-decolator-deprecated] https://docs.python.org/ja/3.7/library/asyncio-task.html#asyncio.coroutine
.. [#fork-asyncio-issue] https://bugs.python.org/issue21998
