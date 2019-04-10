sitemap の追加
==============

:date: 2019-04-10
:tags: SEO, ブログ, Pelican, Google Analytics
:category: 運用

sitemap とかを追加してみたので，それの備忘録．

sitemap 周辺ファイルの追加
--------------------------

まず Pelican で sitemap を生成させる．これは ``sitemap`` プラグインを追加するだけでいい:

.. code-block:: diff

    PLUGINS = [
      ...
  +   'sitemap',
    ]

後は ``SITEMAP`` に設定を書く．以下の感じのことを書いた::

  SITEMAP = {
    'format': 'xml',
    'priorities': {
      'articles': 0.7,
      'indexes': 0.5,
      'pages': 0.6,
    },
    'changefreqs': {
      'articles': 'monthly',
      'indexes': 'daily',
      'pages': 'monthly',
    }
  }

これで，ビルドすると ``output/sitemap.xml`` が作成される．後は， ``robots.txt`` で sitemap の場所を通知してやる． ``robots.txt`` は直下に置かなければいけないので， https://github.com/mizunashi-mana/mizunashi-mana.github.io の方に追加した．普通に以下の感じのことを書く::

  User-agent: *
  Disallow:
  Allow:

  Sitemap: https://mizunashi-mana.github.io/blog/sitemap.xml

これでおk．

Google Search Console の登録
----------------------------

後 Google Search Console に登録する． Search Console のページに行って https://mizunashi-mana.github.io でプロパティを追加すると，所有の確認画面が出てくる．そのサイトを所有しているかの確認方法はいくつかあって，一番簡単なのは Google Analytics が既に導入されているならそれでやるのがいいっぽい．https://mizunashi-mana.github.io は Google Analytics は動いてないので，別の方法でやる必要がある．簡単なのは HTML タグに meta 情報を追加する方法だったけどなんかうまくいかなかったので， HTML ファイルを追加するやつを試した．

所有の確認画面から HTML ファイルをダウンロードしてその名前でアップロードするだけ．それで確認ボタンを押せばいける．ついでに， ``robots.txt`` の更新リクエストも送っておく．これは， `robots.txt テスター <https://www.google.com/webmasters/tools/robots-testing-tool>`_ から行える．更新したいサイトを選んで，送信ボタンを押すと，更新される．後， sitemap の送信もしておく． Google Search Console の「インデックス」 > 「サイトマップ」に行って，画面に sitemap の URL を入力すればいい．後は数日待つ．

後， Google Analytics と Google Search Console を連携できるので，連携しておく． Google Analytics の集客メニューに Search Console ページへのリンクがあるので，それを踏んで Search Console のプロパティを対応づけする (どういうことやったのか忘れた) ．

数日後には Google Search Console で色々な情報が見れるようになっている．なお，このページは検索対象にちゃんと含まれているのか？ みたいな疑問があれば， URL 検査からチェックしてみるといい．そうすると， Google 検索のためにインデックスされているページ情報が見れる．

まとめ
------

正直 SEO 対策しても改善に取り組む姿勢が僕に無いんで，グーグルに情報提供してるだけみたいにもなってるが，まあ何もしなくてもグーグルに情報行ってるんで多少はね？ それにこういう系は見たいって思った時に導入して即見れるとかでは無いし，自分でやらないと知見溜まらないしね．たまには覗いていくか〜みたいな感じ．
