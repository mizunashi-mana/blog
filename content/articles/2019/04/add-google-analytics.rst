Google Analytics の導入
=======================

:date: 2019-04-08 12:30
:tags: Pelican, ブログ, Google Analytics, SEO
:category: 運用

Google Analytics を導入したので，やったことをまとめた．

トラッキング ID の設定
----------------------

既に Analytics のアカウントは持っていたので，そのアカウントのプロパティを追加してトラッキング ID を入手するだけ．以下の手順を行うだけだった:

1. https://analytics.google.com/analytics の管理ページ表示

2. 「プロパティを作成」ボタンを押す．

3. プロパティの項目を埋めていく．大体以下の感じ:

   * ウェブサイトの名前: GitHub Pages
   * ウェブサイトの URL: https://mizunashi-mana.github
   * ウェブサイトの業種: その他

     * なお，業種を設定すると目標テンプレートというものを利用できるらしい．詳細は， https://support.google.com/analytics/answer/1032415 を参照．

   * ウェブサイトのタイムゾーン: 日本 (+09:00)

4. そうするとトラッキング ID のページに移動する．表示されているコードは無視して，トラッキング ID だけメモる．

   * Pelican の Flex テーマが対応しているため，特にコードの埋め込みとかは必要ない．

後はメモった トラッキング ID を Pelican 側で設定するだけ． publish した時のみ設定されるよう ``publichconf.py`` に以下を足す::

  GOOGLE_ANALYTICS = "UA-XXXXXXXXX-X"

これで Google Analytics が動くようになる．

プライバシーポリシーの変更
--------------------------

Google Analytics の `利用規約 <https://www.google.com/analytics/terms/jp.html>`_ には，「7. プライバシー」の節に以下のような定めがある．

* 導入するサイトのプライバシーポリシーを公開すること．
* プライバシーポリシーで，データ収集のため Cookie を使用していることを必ず通知すること．
* プライバシーポリシーで，Google Analytics を使用していること及び Google Analytics でデータが収集、処理される仕組みについても必ず開示すること．これは， https://www.google.com/intl/ja/policies/privacy/partners/ などのリンクを目立つように示すことで可能．

事前に公開通知する旨も出していたので，それを実施する形で，プライバシーポリシーに以上の事柄に違反しないよう Google Analytics の説明を追加した．ついでに整形も行なった．差分は， https://github.com/mizunashi-mana/blog/pull/20/commits/8da9a0c91ee5d52e9d0fa62204adadede726493b#diff-ad32853aff6dff0fd8fb93798300aec0 から確認できる．

まとめ
------

Google Analytics をブログに導入した．後は，検索ワードとかを随時見ながら， Google Analytics の知見をためて，ブログを改善していけたらと思う．
