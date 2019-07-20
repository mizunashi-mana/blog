DokuWiki の構文拡張プラグインの作り方
=====================================

:date: 2019-07-20 22:00
:tags: PHP, Wiki, DokuWiki, プラグイン
:category: ツール

DokuWiki で Markdown が書けるプラグインを結構前に作った．で，たまにメンテしてるんだけど，そのメンテのために色々備忘録を残しておこうと思ったので，そのメモ．

DokuWiki のプラグイン作成
-------------------------

DokuWiki のプラグインの作成方法は， https://www.dokuwiki.org/devel:plugins に大体まとまっている． DokuWiki では，以下の種類のプラグインを書ける．

構文拡張プラグイン
  文法を拡張するプラグイン．

動作拡張プラグイン
  DokuWiki の動作を拡張するプラグイン． Wiki のページを作った際にテンプレートを適用したりできる．

管理者向けプラグイン
  管理者の操作を拡張するプラグイン． Wiki のアップグレード画面を追加したりできる．

ヘルパープラグイン
  他のプラグイン向けに関数を提供したりできる．

表示拡張プラグイン
  DokuWiki の表示を拡張するプラグイン．

API拡張プラグイン
  DokuWiki の API を拡張するプラグイン．どういうことができるのかはあんまり分かってない．

認証拡張プラグイン
  新たに認証方式を追加できるプラグイン．

CLI拡張プラグイン
  コマンドを新たに追加できるプラグイン．どういうことができるのかはあんまり分かってない．

各プラグインはそれぞれの名前にあった PHP ファイルを用意し，専用の抽象クラスを継承した具象クラスを作成することで，作成できる．今回対象にするのは構文拡張プラグイン．プラグインは，管理者ページの拡張機能管理画面から管理できる．インストールは， DokuWiki のパッケージインデックスから持ってくる方法とアーカイブファイルからインストール方法がある．

なおプラグインのファイル構造は， https://www.dokuwiki.org/devel:plugin_file_structure に載ってる通り．必須なのは ``plugin.info.txt`` で，そこにプラグイン名や情報を書く．後は ``conf`` ディレクトリに設定項目を書いたりする．

DokuWiki のパッケージインデックスは実はそんなに高等なものではなくて，単なる `本家のDokuWiki <https://www.dokuwiki.org>`_ にプラグインのページを探すだけ．プラグインの著者はアーカイブファイルのダウンロード URL を作成し，そのページに登録しておく．プラグインのインストールはそこから行われる．

構文拡張プラグイン
------------------

構文拡張プラグイン (Syntax Plugin) の作り方は， https://www.dokuwiki.org/devel:syntax_plugins に書いてある通り， ``syntax.php`` という名前のファイルに ``DokuWiki_Syntax_Plugin`` クラスを継承して ``syntax_plugin_[プラグイン名]`` という名前のクラスを作る．このクラスは次のメソッドを実装する必要がある:

.. code-block:: php

  <?php

  define('PLUGIN_NAME', 'example');

  class syntax_plugin_example extends DokuWiki_Syntax_Plugin {
    // パースの仕方を制御するための情報を返す．
    // https://www.dokuwiki.org/devel:syntax_plugins#syntax_types に一覧がある．
    public function getType() {
      return 'protected';
    }

    // 追加する構文の種類を制御する．
    // 指定しない場合，インライン構文として認識される．
    public function getPType() {
      return 'block';
    }

    // パースの優先度を設定する．
    // 標準の優先度は https://www.dokuwiki.org/devel:parser:getsort_list に載ってる
    public function getSort() {
      return 69;
    }

    // 構文がマッチする条件を登録する．
    public function connectTo($mode) {
      $this->Lexer->addEntryPattern('<example>(?=.*</example>)', $mode, 'plugin_' . PLUGIN_NAME);

    }

    // 構文が終了する条件を登録する．
    public function postConnect() {
      $this->Lexer->addExitPattern('</example>', 'plugin_' . PLUGIN_NAME);
    }

    // マッチした内容を前処理する．
    public function handle($match, $state, $pos, Doku_Handler $handler) {
      switch ($state) {
        case DOKU_LEXER_UNMATCHED:
          return [
            'render' => true,
            'match' => $match,
          ];
        default:
          return [
            'render' => false,
          ];
      }
    }

    // 前処理したデータをレンダリングする．
    public function render($format, Doku_Renderer $renderer, $data) {
      if (!$data['render']) {
          return true;
      }
      $match = $data['match'];

      $renderer->cdata($match);

      return true;
    }
  }

この場合， ``<example>...</example>`` という構文が追加され，その中身をそのまま表示するようになる．

レンダラのAPI
-------------

``render`` メソッドが受け取る ``$renderer`` は，それぞれのレンダモードに対応したレンダリングを行う API をいくつか持っている． API は， https://xref.dokuwiki.org/reference/dokuwiki/nav.html?inc/parser/renderer.php.html#doku_renderer に載ってる:

* table
* list
* horizontal line
* strong

など， HTML のいくつかのタグに対応するレンダAPIがあり，こいつらを使うと DokuWiki で使われてるスタイルに合わせたレンダリングができる．

レンダラは内部でバッファを持っていて，各APIはそれを更新していく．そいつに直接書き込むこともできる::

  $renderer->doc .= 'something';

例えば XHTML モードの場合， https://xref.dokuwiki.org/reference/dokuwiki/nav.html?inc/parser/xhtml.php.source.html#l315 を見れば分かる通り，大体は API の名前に対応する HTML タグをそのままバッファに追加してるだけ．

実際の DokuWiki がどういうレンダリングをするかは， https://xref.dokuwiki.org/reference/dokuwiki/nav.html?inc/parser/handler.php.source.html#l5 をみれば分かる．例えばリンクの処理部分は， https://xref.dokuwiki.org/reference/dokuwiki/nav.html?inc/parser/handler.php.source.html#l527 になる．URLと名前で区切って内部リンクか外部リンクかなどを判別して適切にレンダAPIを呼び出していく．APIの呼び出しは， 実際にはこのハンドラ内では呼び出されず，呼び出し登録が ``_addCall`` というメソッドで行われているだけで，その後積まれた API 呼び出しの登録が実行されるようになっている．呼び出し登録は文書のデータ構造を把握する際に役に立つらしい [#instruction-for-analysis]_ ．例えば，

::

  ===== Read More =====

  All documentation and additional information besides the [[syntax|syntax description]] is maintained in the DokuWiki at [[doku>|www.dokuwiki.org]].

のような DokuWiki の文書があった場合，

::

  array(
    array('document_start', array()),
    array('header',         array('Header', 2, 0)),
    array('section_open',   array(2)),
    array('p_open',         array()),
    array('cdata',          array("\nAll documentation and additional information besides the ")),
    array('internallink',   array('syntax', 'syntax description')),
    array('cdata',          array(' is maintained in the DokuWiki at ')),
    array('internallink',   array('doku>', 'www.dokuwiki.org')),
    array('cdata',          array('.')),
    array('p_close',        array()),
    array('document_end',   array()),
  )

みたいな呼び出し登録が生成される．

まとめ
------

DokuWiki のプラグイン開発ってかなり狭い領域だと思うけど，まあ誰かの参考になればという感じで．公式にも色々ドキュメントがあるのでのぞいてみると良さそう．て感じで．

.. [#instruction-for-analysis] 実際にレンダリングをしてしまうと単なる文字列になってしまい，文書の構造の情報がなくなってしまうが，呼び出し登録は少なくともレンダラの API の呼び出し順序などが見れる．
