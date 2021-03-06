OSS ライセンスについての色々
============================

:tags: ライセンス, OSS, GPL
:category: 規範

Haskell-jp でちょっと話題になったので， OSS ライセンスについての個人的な色々を書いておこうと思う．なお，僕は OSS にも著作権にも明るくないので，この記事は多数間違いを含む可能性がある．それぞれ僕の独自の解釈の可能性があるので，そこは注意してほしい．

OSS ライセンスについて
----------------------

そもそもライセンスとは，要は許可証で，何かを使用する際に，その配布者がそれを使って何をして良いか何をしてはいけないかを書いた文書のことだ．配布者が「それを守れるなら使っていいよ」と，使用許諾を出すときに出す契約書のようなものだ．

OSS ライセンスは，その文書のテンプレのようなもので，現在は `Open Source Initiative <https://opensource.org/>`_ ，通称 OSI が承認したものを指す事が多いんじゃないだろうか．そのリストは https://opensource.org/licenses/alphabetical から見れる．有名な `オープンソースの定義 <https://opensource.org/osd>`_  ( `注記付き版 <https://opensource.org/osd-annotated>`_ ) で決められた制約を満たしているか精査されたものが，承認される．大雑把に言えば，

* 再頒布を制限しない．再頒布に対して報酬や制約を要求しない．
* ソースコードの頒布を制限してはいけない．
* 同じライセンスで追加コードを一緒に頒布するのを妨げてはいけない．
* 特定の個人やグループを差別して，あるグループだけに特別な制約をかけてはいけない．

という感じだ．僕が観測しているよく使われている OSS ライセンスは，以下のもの:

GNU General Public License Version 3.0
  通称 GPL ．コピーレフトと呼ばれる特徴を持つ [#copyleft-is-minor]_．古くは， GPL Version 2.0 がよく使われていたのだが，そのままじゃ色々古いよねという事で作られた．コピーレフトは後ほど説明するが，この特徴はライブラリを組み合わせるとき非常に都合が悪い．なので，使用者に不自由を強いたくないなら，他のライセンスを使うことをお勧めする．

  なお，歴史的事情により多くの主要なプログラムは GPL Version 2.0 を使っている．この辺も後ほど説明する．

  採用してるソフトウェアは， Ansible / GCC / Moodle など

  https://opensource.org/licenses/GPL-3.0 (公式: https://www.gnu.org/licenses/gpl-3.0.en.html)

The 3-Clause BSD License
  通称「修正BSDライセンス」． BSD ライセンスの 3 条項目，いわゆる宣伝条項が GPL と両立しないため，当時その条項が消されたものを使う事が多々あった．これをとりまとめたライセンス．条文がかなり少ない上，制約もかなり緩い． GPL のコピーレフトに対抗して作られたようなもので，現在よく使われてるライセンスの多くは，このライセンスの流れを汲んでいる．

  採用してるソフトウェアは， Golang / d3.js / Redis / googletest など

  https://opensource.org/licenses/BSD-3-Clause

Apache License, Version 2.0
  割と新しいライセンスで， BSD 系の緩すぎる制約を少し見直し，制約を明確化し特許保護の規定を盛り込んだライセンス． GPL を忌避する界隈ではよく使われてる気がする．

  採用してるソフトウェアは， Kubernetes / OpenSSL (v3 以降) / TypeScript / Apache Solr など

  https://opensource.org/licenses/Apache-2.0 (公式: https://www.apache.org/licenses/LICENSE-2.0)

MIT License (Expat License, X11 License)
  ライセンス条項がかなり短いやつ．ほとんど何も制限していない， GPL の当て付けみたいなライセンス． BSD ライセンスの癖のある条項消したら，こうなったみたいな感じ．割とちゃんとライセンスを考えるならオススメしないが， GPL や他のライセンスとの兼ね合いからこのライセンスにせざるを得ないみたいな場合もある．ただ NPM 界隈は，結構脳死でこのライセンスにしてるソフトが多い気がする (付属ライブラリが多い傾向にあるからかな) ．

  採用してるソフトウェアは， Angular.js / VSCode / Twitter Bootstrap など

  https://opensource.org/licenses/MIT

Mozilla Public License Version 2.0
  通称 MPL ．GPL との両立を見据えた特殊なライセンス．他のライセンスとの運用でも， MPL の部分だけは保護するようなライセンスとなる．運用が少し難しいが，新しいライセンスで，制約を明確化し特許保護の規定が盛り込まれているので，コピーレフトな運用がしたいならいいと思う．

  採用してるソフトウェアは， Consul / Boulder (ACME CA) / Mozilla Firefox など

  https://opensource.org/licenses/MPL-2.0 (公式: https://www.mozilla.org/en-US/MPL/2.0/)

他にも著名なのはいくらでもあるけどね．

GPL 汚染
--------

さて， OSS ライセンスの話でよくコピーレフトと非コピーレフトという話が出てくるが，実際問題としてコピーレフトという機能を積んでるのは GPL 系 (GNU Public License 系と MPL) だけだ．そして，最近 GPL はあまり選ばれなくなってきてる．なんで選ばれなくなってきてるかというと，単純に知名度の問題と，皮肉にもその特徴であるコピーレフトの起因がある．ただ，選ばれなくなってきてるとはいえ，ここら辺話さざるを得ない話題なので，まずはここから話していく．

そもそも，一昔前までは GPL というのは OSS ライセンスの代名詞みたいな存在だった．で，多くの主要なプロジェクトはこのライセンスを使っていた．特に， GPL 2.0 は数多くのプロジェクトで使われたわけだけど，このライセンス，かなり癖が強かった．その癖の 1 つが GPL の代名詞と言えるコピーレフトだ．コピーレフトの要点は 2 つだ:

* 再頒布は同じライセンスで行わなければならない．
* 再頒布の際，追加の制限をかけてはならない．

MPL もこの条項を持っているが， GPL はその適用範囲が賛否両論を生んでいる．それは「結合された作品 (Conveying Works)」の再頒布の際も上の制約がかかることだ．つまり，例えば GPL で提供されるライブラリ A をライブラリ B が使う場合， B をコンパイルしてそのライブラリアーカイブを頒布する際は GPL 下で頒布しなければいけないし，ライブラリ B をプログラム C が使う場合やはり実行ファイルは GPL 下で頒布しなければいけないということだ． MPL はその範囲を頒布者が明示的に制限することになっていて，その範囲を超える場所は特に制限はないけど，明示的に制限された場所に関しては MPL の条項に従う必要がある．

再頒布の際追加の制限をかけてはいけないというのは，例えばある頒布元で秘密保持契約を結ばされて秘密裏に頒布されるみたいなことを防ぐためだけど，おかげで GPL 2.0 で配布されたものに追加の制限がかけられず，他のライセンスのプログラムと組み合わせられないみたいなことも多数起きた． GPL 3.0 のプログラムさえ， GPL 2.0 と組み合わせられない．

よって， GPL 下のライブラリに依存してしまうと，そのライブラリを含むライブラリも GPL で頒布になり，そのライブラリを含むプログラムもやはり GPL で頒布になり， GPL は追加制限をかけられないので GPL より制限の強いライセンスでは頒布できない [#image-copyleft]_ ので， GPL の範囲がじわじわ広がっていってそれを止められもしないということになる．これは， GPL を作った人たちにとって狙い通りなわけだけど， GPL とは無縁でいたい人たちからはたまったものではなく，「コピーレフト汚染」もしくは「GPL 汚染」と呼ばれたりする．

後， GPL や MPL では著作権保有者という概念はなく，あるのはこのプログラムは GPL でライセンスされてるという契約だけで，権利を作成者も再頒布者も主張できない．特に， GPL 3.0 では特許についての明確な条項が入ったので，何か権利も主張できないしコードも公開しなければいけない，そんな何かがじわじわ広がっていくことになる．

ただ，注意して欲しいのは， GPL ライセンスが適用される範囲はあくまで結合全体であり， GPL と組み合わせる前提のプログラムを同じライセンスで頒布しなければならないというわけではないということだ．ここでの結合とは，静的 / 動的リンクなどを行ったものを指す．実際にプログラムを頒布しないなら， GPL ライセンスにする必要はない． GPL のプログラムの出力結果なども同様だ．また，私的な範囲の利用も頒布に当たらないため， GPL を適用する必要はない．これは，以下の GPL の FAQ の回答を読むのがいいだろう:

* `(GPLの)及ぶ作品に対し、静的 vs 動的にリンクされたモジュールについて、GPLには異なる要求がありますか?  <https://www.gnu.org/licenses/gpl-faq.html#GPLStaticVsDynamic>`_
* `GPLの及ぶプログラムに対してあるモジュールを追加する場合、わたしのモジュールにもライセンスとしてGPLを適用しなければなりませんか?  <https://www.gnu.org/licenses/gpl-faq.html#GPLModuleLicense>`_
* `一つの組織あるいは会社で複数のコピーを作成して使うことは「配布」となりますか? <https://www.gnu.org/licenses/gpl-faq.html#InternalDistribution>`_
* `自由でないプログラムを開発するために、GNU EmacsのようなGPLの及ぶエディタを使っても良いでしょうか? GCCのようなGPLの及ぶツールを使って自由でないプログラムをコンパイルすることはできますか? <https://www.gnu.org/licenses/gpl-faq.html#CanIUseGPLToolsForNF>`_

なので，結合したものを頒布しないなら， GPL ライセンスを使う必要はない．ただ，注意して欲しいのは，自分が頒布しなくても他人が頒布するかもしれないことに気をつけなければいけないということだ．

ライセンス運用時の注意
----------------------

で，なんで GPL について先に話したかというと，こいつやたらと運用がめんどくさい．これが，最近忌避されてる理由でもある．まあ，他にも幾つか運用に注意が必要なライセンスはあるので，知ってるライセンスの運用方法をちょっとまとめとく．

GPL との共存
::::::::::::

GPL は先に言ったように，追加の制限を設けてはいけない．ただ， GPL より緩いライセンスとの共存は可能だ．多くの場合手間との兼ね合いから，自身のソースコードには GPL より緩いライセンスを使いそのライセンスファイルを置き， GPL 下のライブラリのライセンスファイルを消さないで一緒にソースコードとして含めて頒布すると， GPL 下のライブラリのライセンスファイルの条項により全体が自動的に GPL と解釈されて GPL ライセンスの下での頒布になるという解釈がされる，というのが慣例だと思う．この慣例は，基本的に運用の問題なので， GPL でそういう運用が想定されてるわけではないけど，そうなってるのが現状という感じ．なので， GPL との共存を見据えた基本的な運用方法は，

1. GPL または GPL より緩いライセンスで自身のコードを配布する．
2. コンパイル済みバイナリなどを配布するのなら，インストーラなどでこのプログラムは GPL 下で配布されること，ソースコードのリンク先を表示し，使用者の同意を得る．

みたいなのがいいだろう．ライブラリの場合， GPL のライブラリに依存してることを明記しておくと親切だろうが，別に明記しなくてもいい．とりあえずの絶対条件は，

* GPL より緩いライセンスでコードを配布すること．
* ソースコードをダウンロードできるようにしておくこと

だ．自身でバイナリを配布しないなら，後は再頒布者の責任となるだろう．で，具体的に GPL より緩いライセンスってどういうものがあるのかなんだけど， GPL 2.0 と GPL 3.0 で共存できる範囲が異なり，

* GPL 2.0 or 3.0: LGPL 2.1 / 修正BSD / MIT / MPL 2.0
* GPL 3.0: Apache 2.0 / LGPL 3.0

という感じ． GPL 2.0 より GPL 3.0 の方が制限が強いので， GPL 2.0 と GPL 3.0 は共存できない．逆にいえば GPL 2.0 と共存するなら GPL 3.0 とも共存する．なお， GPL 2.0 には ``or later`` という文面を加えることにより GPL 3.0 でもライセンスとして許容するという慣例がある．この場合， GPL 3.0 扱いにできるので， Apache 2.0 とかとも共存できる．この共存できるというのを， OSS ライセンス界隈では，ライセンス互換性と呼ぶことがある．

MPL の運用
::::::::::

MPL 2.0 は，かなり特殊なライセンスで， GPL と同じようにコピーレフトなんだけど，その効力はファイル単位と規定されてる．なので，基本的にはファイル単位で GPL と同じことを気をつければいい． MPL 2.0 のライブラリに依存するプログラムを作ってそれをコンパイルしたバイナリを配布する場合は， MPL 2.0 のライブラリのファイルは公開しなきゃいけない．ただ，そのファイル以外については特に制約なくどんなライセンスを使ってもいい．なので，かなりコピーレフトの範囲に制限がかかる．

MPL 2.0 を自身のコードに使いたい場合は， https://www.mozilla.org/en-US/MPL/2.0/FAQ/#apply に書いてある通り，ライセンスを適用したい範囲に ::

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.

という文面をコメントで入れればいい．ただ運用上めんどくさい場合，あまり推奨ではないが，適用する範囲を明示した注記をどこか目立つところに書くだけでも良いことになっている [#mpl-notice-exception]_ ．

ところで， MPL 2.0 は， GPL との結合に対して特別の条項を設けている．本来， MPL はその保護下にあるファイルを MPL で提供しなければいけないが， GPL は結合したものを GPL で提供する必要がある．なので， MPL 保護下のファイルは GPL に関してはライセンスを切り替えてもいいことになっている．もっと厳密にいうと，切り替えていいライセンスは `MPL の 1. Definition <https://www.mozilla.org/en-US/MPL/2.0/#definitions>`_ の 1.12. "Secondary License" で述べられている，以下の 4 つだ．

* GNU General Public License, Version 2.0
* GNU Lesser General Public License, Version 2.1
* GNU Affero General Public License, Version 3.0
* 以上のライセンスの上位バージョン

よって， MPL 2.0 は GPL 2.0 ， GPL 3.0 をはじめとする GPL 系のライセンスと互換性があることになる．

デュアルライセンス
::::::::::::::::::

ライセンスは何も単一のものを提供する必要はない． 2 種類のライセンスを提供するデュアルライセンス，または 2 種類以上のライセンスを提供するマルチライセンスも選択肢として，実際に使用されている方法だ． Mozilla Firefox は元々 GPL への切り替え条項がなかった時代は， GPL / LGPL / MPL のトリプルライセンスで提供されていた．この名残で， MPL 2.0 の Secondary License 条項はトリプルライセンス条項とも呼ばれている．

デュアルライセンスというのはよく勘違いされがちだけど， 2 つのライセンスを二重に適用するものではなく，どちらのライセンスを使うか，使用者が選べるライセンス方式だ．これは， GPL 2.0 のライブラリと組み合わせる場合に，片方を緩いライセンスにしておくことで互換性問題を回避するのに便利だ．ただそのようなテクニック以上の意味もある． MPL や GPL は，著作権主張は存在せず代わりにライセンス規則が後々までずっと効いてくる．対して，通常の OSS ライセンスは著作権を主張する代わりに改変や再頒布に対して著作権表記ぐらいしか制約を課さない．この 2 つをデュアルライセンスで提供することで，著作者の権利を主張し尊重したい人，自由にプログラムを作ることを促進したい人はコピーライトの制約の緩いライセンスを， OSS の共同体の一員として特に著作権主張をせずにコントリビュートをしたい人， OSS の公開の場での技術共有に価値があると思う人はコピーレフトなライセンスを選択する，その選択肢を提供することで，使用者の価値観や文化を尊重しながら使用許諾を与える為の方法という面もある．

デュアルライセンスは，使われてるとこでは使われていて， Rust は MIT / Apache 2.0 のデュアルライセンスになっている． Perl は GPL Version 1 以上と Artistic ライセンスとのマルチライセンスだ．

ライセンスの変更
::::::::::::::::

ライセンスは，著作者から使用者への使用許諾だと冒頭で言った．でも， OSS では著作者というのは一定に定まらない場合が多い．パッチの提供があった場合，その人も著作者の一人になる． Apache 2.0 では，コントリビューションに対しての権利が明確に書かれており，そのライセンスで書かれている著作権表記者に権利が移譲されることになるが，それでも著作者の一人であることには変わりない．そう考えた時に，ライセンスというのは，著作権表記者が代表者となり，コントリビュータに「この使用許諾を満たす人があなたの著作物を使いますよ，いいですか？」という条件で契約を結び，使用者に「この使用許諾を満たすなら，使用していいですよ」という条件で契約を結ぶ， 2 者の契約者がいることになる．ライセンスの変更は，この 2 者どちらに対してもの契約条件の変更になる．

契約条件の変更というのは結構大きいことだと思う．なので，ライセンスを変更する際は 2 者に良く説明することが大事だ．特にコントリビュータは，自分の著作物がコントリビュート当時のライセンスで提供されるという契約で，代表者に預けている．なので，今までのコントリビュータと良く話し合って決めるというのが通例だと思う． OpenSSL は 3.0 にするタイミングで， Apache 2.0 にライセンスを変更してるけど，この際は過去のコントリビュータと連絡を取り，連絡が取れなかったコントリビュータの書いた部分は書き直しを行なっている [#openssl-is-relicensing]_．これは，ここまでするのかと驚く人もいると思うけど，個人的にはコントリビュータの作品を尊重し，契約を履行する最良の手段だと思う．

その上でライセンス変更をする際は，ライセンスを変更した理由と差分をどっかにまとめておき， README に一定期間注意書きを追加するのがいいと思う．ただ，ライセンスというのは著作者が出す使用許諾なのだから，契約条件の変更自体は良くあることだし，変えたい理由があるならいつだって変えてもいいと思う．もちろん，その変更は今までのバージョンには適用しないのが通例で，メジャーバージョンの変更ぐらいは良くやられる．大事なことは，契約条件の変更があったことを今までの契約者に伝え，互いに了解することだと思う．

個人的な運用法
--------------

で，以上のことを踏まえて個人的なライセンス運用の際のお気持ちと，最近の運用の仕方を書いておく．

よくライセンス界隈で思うこと
::::::::::::::::::::::::::::

OSS ライセンスでは格式張ったテンプレが横行してるせいで，文面の細部に注意がいきがちなんだけど，本来契約が主で，ライセンスというのは契約において単なる書面の一つにすぎない．よく， OSS ライセンスに対して解釈の議論が起きることがあるけれど，本来契約者の解釈こそが最も大事で，その解釈で契約を取り交わしたのに第三者が契約内容にとやかく言うのはどうなんだろうと思う．出回っているテンプレは，良く契約上取り交す条件があって，互いに信頼しているからこそ，その信頼を壊さない為書いておいた方がいいと先人たちが判断したことが，書かれてるものに過ぎなくて，このテンプレにこだわって契約を蔑ろにする行為は本末転倒だと個人的には思う．

ライセンスを自作するのは，それこそ契約の際相手の不信感を煽るという意味であまり推奨されないが，それでも自分が入れておいた方がいいと思う条件があるなら，その条件をライセンスに含めていいと思うし，その旨をしっかり契約時に説明すればいい．大事なのは，ライセンスに書かれていることではなく契約相手と自分が納得のいく形で契約を行うことだ．そんぐらいの気持ちで付き合っていくのが，いい気がする．逆に，契約というのはライセンスが全てじゃなくて信頼が全てだ．ライセンスは信頼を得るための一角であって，このライセンスの運用が信頼を裏切るものならば，そもそもライセンスを用意する意義はないと思う．そこら辺を意識して，自分なりに運用していけるなら，それがベストなんじゃないかと．

最近良く使ってる方式
::::::::::::::::::::

で，最近個人的に良く使ってる方式は，以下のやつ:

1. 通常は， MPL 2.0 / Apache 2.0 のデュアルライセンス
2. GPL への組み合わせが前提の場合， MPL 2.0 / MIT のデュアルライセンス
3. ライブラリとかではなく特に再頒布を意図していないものは， Apache 2.0 のシングルライセンス

MPL は，ライセンス表記をコメントでファイルごとに書くのが強く推奨されてるけど，運用上それだと不都合も多いし見栄えも良くない．なので， MPL 2.0 / Apache 2.0 の構成にする際は，以下の文面を ``NOTICE.md`` って名前で加えることにしている::

  # Notice

  You must read this notice before distributing any copies of files
  of this project or module.

  ## License

  This project or module is dual-licensed under the Mozilla Public
  License 2.0 (at http://mozilla.org/MPL/2.0/) and the Apache License
  2.0 (at https://www.apache.org/licenses/LICENSE-2.0). You can choose
  which one to use.

  ### The Mozilla Public License Notice

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  By definition 1.4 of the Mozilla Public License 2.0, the above notice
  means all of files of this project or module are included in
  "Covered Software".  If anyone distributes just a copy of a file of
  this project or module by the Mozilla Public License 2.0, he or she
  may add this notice by any reasonable method (e.g. as a comment)
  instead of attached this document.

で， MPL 2.0 の文面を ``LICENSE.MPL-2.0`` ， Apache 2.0 の文面に著作権保持者の名前を入れたものを ``LICENSE.Apache-2.0`` ， SPDX 表記でデュアルライセンスを表す ``Apache-2.0 OR MPL-2.0`` と書かれたものを ``LICENSE.txt`` という名前で置いて， README に NOTICE を読んでと書く感じ．

NOTICE の後半部分に書いてあるのは，「上に書いてあることは，うちのプロジェクトのファイルは MPL 2.0 での "Covered Software" であることを意味しますよ．もし，うちのファイルを一部抜き出して MPL 2.0 ライセンスの下で配布したい場合，上の注記を何らかの形で含めてくださいね」って感じ．

まとめ
------

ライセンスについて，自分が持ってる知識と個人的な解釈，運用方法を書いた．

専門家じゃないので，全然偉そうなことは言えないんだけど， OSS ライセンスのそもそもの意義に立ち返って運用していくのは，大事な気がする．そこら辺，なんかいい感じにやっていきたい．

後，読み直してみて思ったけど，この記事かなり GPL へのネガキャンがひどいので，そこら辺は注意してくれ (これをまとめで書くあたり，確信犯な感じがある) ．

.. [#copyleft-is-minor] よくこの特徴で OSS ライセンスを 2 分している人を見かけるのだが，コピーレフトなんて GPL 系のライセンスぐらいしか設けてないやろ...
.. [#image-copyleft] イメージとしては，最大値と最小値が共に GPL になる．元々， GPL より小さいものを GPL と組み合わせると GPL になり，それより大きくもできない感じ．
.. [#mpl-notice-exception] MPL 2.0 の Exhibit A にその旨が書いてある．
.. [#openssl-is-relicensing] https://www.openssl.org/blog/blog/2017/03/22/license/
