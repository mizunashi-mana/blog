unified 形式パッチの文法について
================================

:date: 2019-04-18 22:00
:tags: diff, パッチ
:category: ツール

パッチファイルの形式にはいくつかあるわけだが， `git` の `diff` で使われている形式について，ちょっと調べたのでそのメモ．

diff 形式
---------

diff コマンドが吐き出す形式は色々ある．主に以下のものがあるらしい [#diffutils-manual-output-formats]_ :

通常の形式
  差分部分だけを表示する， diff のデフォルトの表示形式

コンテキスト形式
  差分部分周辺も含めて，どういう風に変わったかが分かるようになっている，表示形式

side by side 形式
  2列で差分を表示する形式

スクリプト形式
  ed コマンドなどでそのまま実行すれば，差分を適用できる形式

if-then-else 形式
  CPP のマクロで，両方を切り替えられるようにした形式．差分部分を ``#ifdef`` で囲って出してくれ，指定したマクロ変数を定義すると古いのに切り替えられる．

色々あるけど，パッチを送信する際よく使用されるのは，プレーンテキストでも差分を確認しやすいコンテキスト形式だ．コンテキスト形式の中でもいくつか種類があって，主に以下のものがある [#diffutils-manual-contexts]_ :

(狭義の) コンテキスト形式
  patch コマンドの標準形式っぽい．追加 / 削除に加えて，変更を表す表記がある．

unified 形式
  ``git diff`` で使われている形式．追加 / 削除しかないやつ．

他にもオプションを指定することで表示を変えられるが，主要なのは上の 2 通りだと考えていいと思う．今回は， unified 形式のみを見ていく．

unified 形式の構文
------------------

コンテキスト形式の差分は，ヘッダ情報とハンク (hunk) と呼ばれる差分を表すコードブロックの集まりから為っている．ヘッダ情報は，以下の形式になる::

  --- from-file from-file-modification-time
  +++ to-file to-file-modification-time

まあ書いてある通り．例えば，以下のような感じ::

  --- dir/test.c.orig     2019-04-15 16:48:32.000000000 +0900
  +++ dir/test.c          2019-04-08 12:40:22.000000000 +0900

なお， ``diffutils`` のマニュアルには書いてないんだが， ``git diff`` は変更時間を出さないので，変更時間は書かなくてもいいのかもしれない．ハンクは以下の形式になる::

  @@ from-file-line-numbers to-file-line-numbers @@
  line-from-either-file
  line-from-either-file
  ...

``file-line-numbers`` は， ``start, count`` の形式をしている． ``start`` は開始位置， ``count`` は開始行からの表示行数になる．それぞれの行は，

* 特に変更がないところは空白 1 つ
* 追加した行は ``+``
* 削除した行は ``-``

を先頭につける．例えば，以下のような感じ::

  @@ -1,5 +1,5 @@
   #include <stdio.h>

   int main(void) {
  -  printf("Hello, World!\n");
  +  puts("Hello, World!");
   }

注意して欲しいのは，位置も行数も差分ではなく表示されている行から換算するってこと．上の例だと，差分があるのは4行目だけど ``start`` は 1 で， ``count`` も変更される前のファイルの表示されてる行数 5 になる．なお，この行数を間違えると，パッチコマンドでは以下のエラーが出る::

  $ cat test.c.patch
  --- test.c.orig 2019-04-18 21:45:33.000000000 +0900
  +++ test.c      2019-04-18 21:45:07.000000000 +0900
  @@ -1,4 +1,5 @@
   #include <stdio.h>

   int main(void) {
  -  printf("Hello, World!\n");
  +  puts("Hello, World!");
   }
  $ patch test.c.orig < test.c.patch
  patching file test.c.orig
  patch: **** malformed patch at line 9:  }

エラーメッセージは分かりにくいが， 9 行目の ``}`` でエラーが出ているのは， ``from-file-line-numbers`` で 4 行のハンクだと言っているのに 5 行目があるじゃないかと怒っているんだと思う．なお，一行以下のハンクの場合，カウントの部分は書かなくていいらしい::

  @@ -1 +1 @@
  -aaa
  +aaaa

それから空ハンクはそこでハンクが終了することを示すらしい．これは何処で使われてるんだろね．

まとめ
------

``git diff`` で使われてるパッチ形式を調べた．これで手動でパッチ作成とかできそう．また一つ賢くなってしまった．

.. [#diffutils-manual-output-formats] https://www.gnu.org/software/diffutils/manual/diffutils.html#Output-Formats
.. [#diffutils-manual-contexts] https://www.gnu.org/software/diffutils/manual/diffutils.html#Context
