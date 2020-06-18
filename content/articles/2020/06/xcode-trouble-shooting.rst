XCode でビルドするときに引っかかったところ
==========================================

:tags: XCode, Objective-C, iOS, ビルド
:category: ツール

XCode を使ってる時，ちょっと検索しにくい問題に2件当たったので，備忘録として残しておく．

Undefined symbols for architecture i386
---------------------------------------

iOS 用の Objective-C のプロジェクトをビルドしてる際，なぜかリンクに失敗し，ある特定の framework について，

::

  Undefined symbols for architecture i386:
    "_OBJC_CLASS_$_XXX", referenced from:
    objc-class-ref in XXX.o

というエラーが全てのシンボルについて出ていた．で，このリンカエラーが起こる前に，次のようなメッセージが出ていた::

  Ignoring file XXX.framework/XXX, missing required architecture i386 in file XXX.framework/XXX

僕は，iOS プログラミングはほぼ初体験だったので，そもそも framework というのがどういうものなのか知らなかったんだけど，こいつはライブラリアーカイブやヘッダ情報などが詰め込まれているものらしい．ライブラリはアーキテクチャごとに用意されていて，どうやら今回の場合，この framework に i386 アーキテクチャが入っていないということらしい．

ところで，件の framework は他のプロジェクトを自分でビルドして持ってきたものだった．自分でビルドしたものに必要なアーキテクチャ用のライブラリアーカイブが入っていないのは不思議だ．で，いろいろ調べてみると，どうやら今回は次のような原因によるものだった (と思われる):

* Debug ビルドでは，XCode はデフォルトで必要なアーキテクチャ用のライブラリしか作らない
* 対象としてる iOS のバージョンが違った

一つ目は，どうやらいろんな人が詰まる問題みたいで，"Build Settings" の "Build Active Architecture Only" という設定が問題になる．この設定は，幾つかの標準アーキテクチャ用のライブラリしかビルドしないという設定で，デフォルトで Debug ビルド時は YES，Release ビルド時は NO になっている．今回はもう一つのプロジェクトの方を Debug ビルドしていて，そのプロジェクトの設定はデフォルトのままだったので，標準アーキテクチャしか framework に入っていなかった．なお，framework がどのアーキテクチャ向けのものなのかは，

::

  $ xcrun lipo -info XXX.framework/XXX
  Architectures in the fat file: XXX are: armv7 armv7s i386 arm64

で確認できる．世の中で配布されている framework は結構アーキテクチャに抜けがあったりするっぽいので，``missing required architecture`` と言われた場合は，まずは上のコマンドで対応してるアーキテクチャを確認してみると良いと思う．

さて，今回は両方 Debug ビルドしていたので，両方標準アーキテクチャが同じなら実は Release ビルドしない限り問題は起きなかったはずだ．では，なんで怒られたかなんだけど，実はいまいち原因が分かっていない．おそらく対象の iOS バージョンが違ったのが原因で，標準アーキテクチャが変わったんじゃないかと思うんだけど，実は詳しく調べてないので細かい原因は判明してない．

とりあえずめんどかったので，今回は framework を作るプロジェクトの方の，Debug ビルド時の設定で，"Build Active Architecture Only" を YES から NO に変更し，一通りのアーキテクチャに対応させることにした．基本的な対応は Release ビルドを行うことだと思う．ただ今回は Debug ビルド時のロジックなども欲しかったのでそういった対応をした感じ．

Extra info about plist: ACL=<not found>
---------------------------------------

さて，上の設定でビルドをしてアプリをシミュレータで起動させてみると，今度は次のようなエラーが表示された:

  This app could not be installed at this time.

  Could not install at this time.
  Failed to load Info.plist from bundle at path ~/Library/Developer/CoreSimulator/Devices/XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX/data/Library/Caches/com.apple.mobile.installd.staging/temp.A3gmX5/extracted/Test.app/Frameworks/XXX.framework; Extra info about plist: ACL=<not found>

このエラーは Run 時に，アラートダイアログで通知されて，``Details`` / ``OK`` というボタンが表示される．とりあえず，``Details`` の方を押してみると，次のようなことを言われる::

  Domain: MIInstallerErrorDomain
  Code: 35
  User Info: {
      FunctionName = "-[MIBundle _validateWithError:]";
      LegacyErrorString = PackageInspectionFailed;
      SourceFileLine = 128;
  }

これでは何も分からないが，どうやら先ほどのもう一つのプロジェクトでビルドした framework の ``Info.plist`` がおかしそうな印象を受ける．ただ，実はこの framework は他のアプリに組み込んで動かしてみたことがあって，その時はこういうことは無かった．

で，何が悪かったのかだけど，この framework を追加する際の設定が悪かった．framework をプロジェクトに追加する際，その埋め込み方が設定できる．今回は，これが ``Embed & Sign`` になっていた．これも詳しい原因はちょっと調べてないんだけど，Debug ビルドで作った signing とか何もされてないやつを，embed しようとすると上のようなエラーになるんかな？

今回は特に embed とかする必要ないし，単にシミュレータで動作確認したかっただけなんで，``Do Not Embed`` にすると無事起動するようになった．

まとめ
------

単にビルドして動かしたかっただけなのに，この2つの問題解決するだけで結構時間かかっちゃった．後，Objective-C 何も分からん．今回は以上です．
