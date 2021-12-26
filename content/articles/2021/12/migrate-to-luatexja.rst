upLaTeX から LuaTeX に移行する
=====================================

:tags: TeX, フォント, 文書作成
:category: ツール

今まで日本語文書作成で upLaTeX を使っていたのだが，体験が微妙だったので最近 LuaTeX に移行した．特に数式フォントとして，STIX2 を使っていたのだが，文字が崩れたりするのをなんとかしたくて，``unicode-math`` を採用するために移行したというのが大きい．そこまで大変でもなかったのだが，詰まるポイントはいくつかあったのでその備忘録．

移行したのは https://github.com/mizunashi-mana/proglang-notes なので，最終形見たかったらこちらをどうぞ．

構成の変更
----------------

今までは，upLaTeX と ``dvipdfmx`` を使っていて， ``jsbook`` をクラスに使った構成にしていた．

* ビルドは， ``latexmk`` 経由で ``uplatex`` と ``dvipdfmx`` から PDF 生成
* フォントは，デフォルト Latin Modern で，日本語は IPAex の埋め込み，数式は STIX2 Type1

みたいな感じだ．まあ良くある構成．これを， ``ltjsbook`` クラスを使い

* ビルドは， ``latexmk`` 経由で ``luatex`` から PDF 生成
* フォントは，デフォルトと数式は STIX2 OpenType，日本語は原ノ味

という構成にした．また，今までは ``stix2-type1`` パッケージを直接使って Unicode 文字経由の数式記号マクロを使っていたが，これを ``unicode-math`` 経由で使うようにした．

Latexmk の設定
--------------------------------

まず ``latexmk`` の設定を変更する．今まで ``uplatex`` / ``dvipdfmx`` を使っていたのを， ``luatex`` を使うよう変更した．設定ファイルは以下:

.. code-block:: perl

    #!/usr/bin/env perl

    use v5.10;
    use experimental qw(smartmatch);

    # tex options
    $lualatex     = 'lualatex -shell-escape -synctex=1 -interaction=nonstopmode';
    $pdflualatex  = $lualatex;
    $biber        = 'biber %O --bblencoding=utf8 -u -U --output_safechars %B';
    $bibtex       = 'bibtex %O %B';
    $makeindex    = 'mendex %O -o %D %S';
    $max_repeat   = 5;
    $pdf_mode     = 4;

    $pvc_view_file_via_temporary = 0;

    # default preview
    given ($^O) {
        when (/MSWin32/) {
            $pdf_previewer = 'start';
        }
        when (/darwin/) {
            $pdf_previewer = 'open';
        }
        default {
            $pdf_previewer = 'evince';
        }
    }

    # local config
    $local_latexmkrc_path = './.latexmkrc.local';
    require $local_latexmkrc_path if -e $local_latexmkrc_path;

``$pdf_mode = 4;`` がポイント． ``latexmk`` の ``$pdf_mode`` は，デフォルトの PDF 生成モードを指定できる．指定できるのは以下のモード [#latexmk-manual]_:

* 0 の場合， ``$latex`` コマンドを実行するだけ．PDF は生成しない
* 1 の場合， ``$pdflatex`` コマンドによって PDF を生成する
* 2 の場合， ``$latex`` コマンドを実行した後， ``$dvips`` コマンドで一旦 PS ファイルを生成し， ``ps2pdf`` コマンドで PS ファイルを PDF ファイルに変換する
* 3 の場合， ``$latex`` コマンドを実行した後， ``$dvipdf`` コマンドで PDF ファイルに変換する
* 4 の場合， ``$lualatex`` コマンドによって PDF を生成する
* 5 の場合， ``$xelatex`` コマンドを実行した後， ``$xdvipdfmx`` コマンドで PDF ファイルに変換する

今までは 3 を指定していたが，今回から 4 を指定している．LuaTeX は基本 pdflatex の後継で，DVI ファイルを経由せず直接 PDF を生成する．この設定により， ``latexmk main.tex`` を実行すると ``main.tex`` から ``lualatex`` コマンドで PDF を生成する．設定はよくあるやつだが， ``-shell-escape`` オプションだけは特別に ``minted`` パッケージのために付けている． ``minted`` 使わないなら付けないほうがいいと思う．

必要なパッケージ
-----------------

LuaTeX で日本語を書くには，以下のパッケージをインストールする:

* ``luatex``
* ``luatexja``

これらは ``collection-luatex``， ``collection-langjapanese`` に含まれている．後今回は

* ``unicode-math``
* ``stix2-otf``

を数式用に使う．後は適当に使うやつをインストールする．

プリアンブル
----------------

LuaTeX-ja では，jsclasses 互換のクラスがいくつか提供されており，和文文書を書く時は以下のものが使える [#ltjsclasses-manual]_．

``ltjsarticle``
    論文・レポート用

``ltjsbook``
    書籍用

``ltjsreport``
    レポート用

元々 ``jsbook`` を使っていたので，今回は ``ltjsbook`` を使う:

.. code-block:: tex

    \documentclass[
        luatex,
        japanese,
        unicode,
        titlepage,
        pdfusetitle
    ]{ltjsbook}

``luatex`` オプションはドライバ判定のため， ``japanese`` は元々 ``babel`` のためにつけていたものの流用， ``titlepage`` はお好みで，他は ``hyperref`` のためのオプションという感じになる．後は適当に使うパッケージを読み込んでおく:

.. code-block:: tex

    \usepackage{luatexja}
    \usepackage{hyperref}
    \usepackage{babel}
    \usepackage{bookmark}
    \usepackage[no-math,haranoaji,deluxe]{luatexja-preset}
    \usepackage{unicode-math}

``luatexja-preset`` は font preset を指定するとフォントをいい感じに諸々設定してくれるやつらしい．今回は haranoaji フォントを使うよう指定している． ``deluxe`` オプションを指定することで，使えるウェイトを増やせる．後は，

.. code-block:: tex

    \setmainfont{STIX Two Text}
    \setmathfont{STIX Two Math}

    % some math fonts not support bold
    \DeclareFontShape{TU}{STIXTwoMath(1)}{b}{n}{<->ssub*STIXTwoMath(1)/m/n}{}
    \DeclareFontShape{TU}{STIXTwoMath(2)}{b}{n}{<->ssub*STIXTwoMath(2)/m/n}{}
    \DeclareFontShape{TU}{STIXTwoMath(3)}{b}{n}{<->ssub*STIXTwoMath(3)/m/n}{}

欧文のテキストフォント，数式フォントに STIX2 を指定する．よく分からないが，フォントシェイプの警告がいくつか出ていたので，フォールバックを明示的に指定している．ここら辺はちゃんと調べたほうが良さそうだが，とりあえずこれでいいかなという．以下が表示サンプル:

アルファベット
    .. image:: {attach}migrate-to-luatexja/alphabet-sample.png
        :alt: アルファベットのサンプル
        :align: center

いろは
    .. image:: {attach}migrate-to-luatexja/iroha-sample.png
        :alt: いろはのサンプル
        :align: center

数式
    .. image:: {attach}migrate-to-luatexja/math-alphabet-sample.png
        :alt: 数式のサンプル
        :align: center

その他
--------

今まで XY-pic を使っていたが，LuaTeX では使えないらしいので，これからは tikz 直接使っていく感じにする．後，数式の記号は今まで ``amssymb``， ``mathtools``， ``stix2-type1`` を併用していたが， ``unicode-math`` に一元化した．多少今まで使っていた記号マクロから名前が変わっていた部分はあるが，基本特に使える文字に変化はなかった． ``unicode-math`` に一元化したことによって，記号の名前か Unicode コードポイントが分かれば ``unicode-math`` マニュアルの `記号一覧 <https://mirrors.concertpass.com/tex-archive/macros/unicodetex/latex/unicode-math/unimath-symbols.pdf>`_ から検索できる．Detexify が対応していないので，名前が何も分からないと不便な場合もあるが，まあ大体は今までより利便性が上がっているはず．

他に， ``amsalpha`` を ``bibliographystyle`` に使っていたが，これが使えなかった．こっちも原因はあまり調べていないが，とりあえず，

.. code-block:: tex

    \bibliographystyle{alpha}

に切り替えて対応している．

まとめ
-----------

LuaTeX に移行する際の諸々を書いた．``\xymatrix`` が使えなくなったのはちょっと残念だが，他はそれほど困っていない．まだ使って日が浅いが，なんとかなるかなあという感じ．今回はそんだけで．では．

.. [#latexmk-manual] https://mirrors.rit.edu/CTAN/support/latexmk/latexmk.pdf
.. [#ltjsclasses-manual] https://mirrors.ibiblio.org/CTAN/macros/luatex/generic/luatexja/doc/ltjsclasses.pdf
