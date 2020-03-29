GHC のノートコメントについてのあれこれ
======================================

:tags: Haskell, GHC
:category: 構想

https://github.com/myuon/ghc-compiler-notes にコントリビュートしたので，それについてと考えてることとか書く．

なお， myuon さんによる紹介記事も参照: https://myuon.github.io/posts/ghc-compiler-notes/

GHC のノートコメント
--------------------

`GHC のコーディングガイド <https://gitlab.haskell.org/ghc/ghc/wikis/commentary/coding-style#comments-in-the-source-code>`_ にも書いてあるが， GHC では長いコメントを書くとき，以下のように主要部分を切り出す運用をしている:

.. code-block:: haskell

  someFunc =
    let v = 42 -- See Note [The Answer To Life]
    in v

  {- Note [The Answer To Life]
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  "The Answer to the Ultimate Question of Life, the Universe, and Everything" is 42.
  After seven and a half million years of calculation, deep thoughts finally output the answer.
  -}

例えば， https://gitlab.haskell.org/ghc/ghc/blob/ghc-8.6.4-release/compiler/prelude/TysPrim.hs#L593 のような感じ．内容は，

* 実装上の注意事項やアドバイス
* 形式的なアルゴリズムの説明
* API の使い方や使われ方

を含む色々． GHC 開発チーム向け以外にも， GHC で Haskell を書く上で重要なことが書いてある．例に挙げた type equality のまとめの他，

The rules for map
  ``map`` の ``RULES`` がどういう挙動をするかの話

  https://gitlab.haskell.org/ghc/ghc/blob/ghc-8.6.4-release/libraries/base/GHC/Base.hs#L1119

The oneShot function
  ``oneShot`` 関数の概要

  https://gitlab.haskell.org/ghc/ghc/blob/ghc-8.6.4-release/compiler/basicTypes/MkId.hs#L1473

Call Arity: The goal
  call arity 解析の概要

  https://gitlab.haskell.org/ghc/ghc/blob/ghc-8.6.4-release/compiler/simplCore/CallArity.hs#L35

Choosing loop breakers
  loop breaker をどのように選ぶかの話

  https://gitlab.haskell.org/ghc/ghc/blob/ghc-8.6.4-release/compiler/simplCore/OccurAnal.hs#L279

Join points
  join points の概要

  https://gitlab.haskell.org/ghc/ghc/blob/ghc-8.6.4-release/compiler/simplCore/FloatOut.hs#L112

など，より Haskell を理解する上で役に立つコメントも多い．

GHC Compiler Notes
------------------

GHC のノートコメントは有用なことが書いてあり， GHC のリポジトリで管理されているためコードとの乖離も少ない．ただ，コードと一緒に書いてある分ノイズも多い． GHC の利用者としてはノートコメントだけを参照したい場合も多く，そこだけ抽出して見やすくできないかとは思っていた．で，半年前くらいに `myuon さん <https://twitter.com/myuon_myon>`_ と `waddlaw <https://twitter.com/waddlaw>`_ さんが，そういうプロジェクトを作っていると言う話を聞いて気になっていた．そのプロジェクトが上で挙げた https://github.com/myuon/ghc-compiler-notes になる．

でちょっと前にノートコメントを参照することがあって，どうせならそのプロジェクトを利用してみるかという感じだったんだが，作りかけで止まっていたっぽかった．やる気と時間があったのでひとまず，完成させてみることにした．最初は結構簡単にいくやろと思っていたが結構難しかった．内部の仕組みは主に，

1. GHC API を利用して Haskell コードをトークン解析する．

2. トークンをパースして，ノートコメントに分割する．

3. 分割されたノートコメントを統合して， reStructuredText 形式で吐き出す (この辺結構怪しい)

という感じ．手間がかかったのが GHC API を利用する部分で， ``Show`` インスタンスがないデータを使わされたり， ``DynFlags`` が色んなとこにでてきたり， lazy I/O を気にしなきゃいけなかったり， export されてない関数を使う必要があったり，まあ色々大変だった．特に今回これをやるまで分かってなかったんだけど， GHC は Haskell ファイルを大体以下の手順で解析するらしい．

1. Lexer をヘッド部分 [#get-options-range]_ まで走らせ，``LANGUAGE`` プラグマや ``OPTIONS_XXX`` プラグマを読み込む．

2. ``CPP`` 拡張が指定されていれば， ``cpp`` を走らせる．

3. Lexer によるトークン解析をする．

4. トークンをパースして， AST に変換する [#parsing-hs]_．

5. 後は色々する．

後，通常 GHC はコメントを読み飛ばすが，

Haddock モード
  Haddock のドキュメントコメントを解析し，それもトークンに含める．

生のトークンを残すモード
  本来なら削除するコメントトークンを，出力するトークンに含める．

という2つのモードも提供されている．今回はこのどちらもを使用した．トークンに分割した後は， https://github.com/myuon/ghc-compiler-notes/blob/master/doc/ParsingNotes.md に沿って大体 regular なパーシングをしていった．

最終的な成果物だが， PR を送った結果， myuon さんが解析結果をいい感じに整形するコードと Sphinx の設定を追加して Read the docs に上げてくれて，今は https://ghc-compiler-notes.readthedocs.io/en/latest/ から見れるようになっている．検索もできるので，ぜひ参照してほしい．

ノートコメントの文法
--------------------

ところでここからが本題なんだが，予想していたことではあるものの，解析は現状あまり上手くいっていない．ノートコメントの範囲を厳密に解析できていないのもそうなんだが，主に文法解釈が壊滅している． GHC のノートコメントは今まで何となく reStructuredText だと思っていたんだけど，少なくとも reStructuredText ではないっぽい．というか，何か形式化されている文法には則っていないんじゃないかと思っている．ちょっと調べた感じでは，この文法に従えというのは特に明記されておらず，事実ノートコメントは色んな文法に則って書かれている．

LaTeX の文法やHaskell Wiki の文法も度々散見されるが，多くはある一定の慣習 (これも時期と書く人によってかなりブレているんだが) に則って書かれてはいるようだ．

Section
  ``*******`` で両側を囲んで，それをさらに ``*`` で囲み，その内側に題を書く．

Sub section
  ``-`` / ``~`` などで下線を引く．

Code block
  インデントする．

という感じだ．大体は reStructuredText より Markdown の文法に近いようであるが， `古典的な Markdown <https://daringfireball.net/projects/markdown/syntax>`_ では ``~`` による下線を認めてない辺り，古き良き Markdown 寄り独自のマークアップの感じがする．この辺何か知っている人がいれば，色々教えてもらいたい．

さて，せっかく労力をかけたのにこのザマではちょっと悲しい．なので，この問題をなんとかする手を色々考え中だ．基本的な方針は変えようがなくて，「 GHC チームとコンタクトを取り， GHC のソースを修正する」だ．ただ，どのような提案をするかが問題になる．現状考えていることは，

1. 形式的な仕様があるのか，形式的な仕様を策定 (既存のに乗っかる / 新たなマークアップ定義をする) しないかと提案する．

   * 特に，コードブロックと引用，サブコンテンツは区別してほしい．

2. 明らかな現状の間違い (下線がないノートコメント / コードブロック表記が独自) を修正するリクエストを投げる．

3. 形式的な仕様が策定されたらそれに合わせて GHC Compiler Notes を修正．既存の GHC コードもちょっとずつ修正するリクエストを投げていく．

という感じだ．これを Haskell Cafe か ghcdev のメーリングリストに投げてみようかなと考えている．気がかりなのは，これはあくまでプログラムによる自動解析のための修正だということだ．本来コメントは分かりやすさが正義だし，見栄えを気にするならそれこそ GHC Wiki に書くべきだろう．その辺の兼ね合いはどうなんだろうという気はしなくもない．

まあ，提案が reject されればそれはそれだし，とりあえず提案してみようかなという気にはなっている．

まとめ
------

というわけで， GHC Compiler Notes にコントリビュートして，あれこれ考えた話を書いた．時間があれば，提案の文面考えて適当に投げていきたい．以上．

.. [#get-options-range] ほぼ ``module`` キーワードまでと思って良い．厳密には， ``CPP`` とかが入ったりすると違ったりするんだが．
.. [#parsing-hs] この時完全に構文が解析されるわけではなくて，演算子の優先順位の解決とかはリネーム時に行われる．
