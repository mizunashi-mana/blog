Sentry でエラー監視
===================

:date: 2019-05-02 18:00
:tags: ブログ, 監視, JavaScript
:category: 運用

クロスプラットフォームのエラー監視を行う Sentry というサービスがある．ブログに試しに導入してみたので，それについて書く．

Sentry について
---------------

Sentry は，提供されている SDK を PHP コードや JavaScript コードに埋め込むことで，エラーを Sentry に送り，管理画面から確認できるようにするサービスだ．コードが https://github.com/getsentry/sentry で公開されていて，オンプレでも動かせる． https://sentry.io/ にデプロイされていて，今回はそれを使っている．

例えば JavaScript でエラーが取れると，以下のような情報が見れる [#hide-ip-address]_:

.. image:: {attach}add-sentry-monitoring/sentry-web-image.png
   :alt: Sentry の Web画面では，エラーが起きた IP / User-Agent などを見れる．

管理画面ではエラーを共有したり，コメントをつけたり，似たエラーを見れたり，色々充実してる．今回は使用していないが，ユーザーにフィードバックを求めるダイアログを表示し，そのフィードバックを管理したりもできる．

内部の仕組みは，詳しく見てないけど， JavaScript の場合 `window.onerror <https://developer.mozilla.org/en-US/docs/Web/API/GlobalEventHandlers/onerror>`_ とかで監視してるっぽい？

Sentry の導入
-------------

では， Sentry を導入してみる．と言っても，そこまで複雑なことをする必要はなくて，大体以下の手順をやればいい:

1. (アカウントが無ければ) https://sentry.io/ に Sign Up する．
2. プロジェクトを作る．
3. SDK をインストールする．
4. セキュリティトークンを入手し，設定する．

アカウントを作ると，プロジェクト作成画面に移るので，監視したいプロジェクトの種類を選択し，名前を入力してプロジェクト作成を行う．後は， ::

  npm install -D @sentry/browser

して， Installing Instructions からセキュリティートークンを入手し，書いてある通りに設定する::

  import * as Sentry from "@sentry/browser";

  Sentry.init({
    dsn: 'https://xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx@sentry.io/xxxxxxx',
  });

試しに，エラーを起こしてちゃんと情報が取れるか確認してみると良い::

  setTimeout(() => { throw new Error("sample error"); }, 0);

これで，管理画面の Issues にエラーが表示されれば大丈夫．

まとめ
------

まだ本格的に使い始めて無いので，使い勝手は分からないけど，色々便利そうではある．

世の中色々便利なサービスはあるんだなあって感じだ．こういうの使ってみないと知見溜まらないしね．そういう感じで．

.. [#hide-ip-address] IP アドレスは隠してる．
