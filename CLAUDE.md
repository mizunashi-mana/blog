# 開発ガイド

このドキュメントは、Claude Codeが効率的にこのブログプロジェクトを理解し、開発を支援するための指針です。

## プロジェクト概要

このプロジェクトは、Pelican（Python製静的サイトジェネレーター）をバックエンドに、モダンなフロントエンド技術スタックを組み合わせたブログサイトです。

### 技術スタック

**フロントエンド**
- TypeScript (メインエントリー: `theme/src/index.ts`)
- SCSS (CSS記述)
- Webpack (モジュールバンドラー)
- KaTeX (数式レンダリング)
- Tippy.js (ツールチップ)
- Font Awesome (アイコン)
- Sentry (エラー監視)

**バックエンド/ビルド**
- Pelican (静的サイトジェネレーター)
- Python (プラグイン開発)
- uv (Python依存関係管理)
- devenv (Nix開発環境管理)

**品質管理・テスト**
- ESLint + Prettier (JavaScript/TypeScript)
- Stylelint (SCSS)
- Ruff (Python)
- Playwright (E2Eテスト)

## ディレクトリ構造

```
blog/
├── content/           # コンテンツ（記事、ページ、アセット）
│   ├── articles/      # ブログ記事（年/月別）
│   ├── asset/         # 画像・ファビコン
│   └── pages/         # 固定ページ
├── theme/             # テーマファイル
│   ├── content/       # Pelicanテンプレート・静的ファイル
│   └── src/           # TypeScript/SCSSソースコード
├── plugins/           # Pelicanプラグイン
│   ├── custom/        # カスタムプラグイン
│   └── official/      # 公式プラグイン
├── output/            # ビルド出力（静的サイト）
├── tests/             # Playwright E2Eテスト
└── 設定ファイル群
```

## 開発コマンド

### 基本的な開発コマンド

```bash
# 開発サーバー起動（ホットリロード対応）
make devserver

# フロントエンド開発ビルド
make js-build-dev

# 本番ビルド
make html

# 本番フロントエンドビルド
make js-build

# テスト実行
npm test
```

### Linting・フォーマット

```bash
# CSS lint
npx stylelint "theme/src/**/*.scss"

# Python lint/format
uv run ruff check .
uv run ruff format .

# TypeScript lint
npx eslint theme/src/

# すべての対象ファイルのフォーマット
npx prettier --write .
```

## コーディング規約

@docs/CodingGuideline.md

詳細なコーディング規約については、[docs/CodingGuideline.md](docs/CodingGuideline.md) を参照してください。

### 概要

- **TypeScript/JavaScript**: ESLint Flat Config + Prettier（4スペースインデント、セミコロン必須、シングルクォート、絶対パス強制）
- **SCSS**: Stylelint + Prettier（BEM命名規則、SCSS変数使用、テーマ対応）
- **Python**: Ruff（Black準拠、型ヒント推奨、docstring必須）
- **Git Hooks**: devenv.nixによる自動フォーマット（commit時）

## 重要な設定ファイル

### webpack.common.config.js
フロントエンドビルドの基本設定。TypeScript、SCSS、アセット処理を定義。

### playwright.config.mjs
E2Eテスト設定。ベースURL、ブラウザ設定、webServer設定を含む。ES Module形式で記述。

### eslint.config.mjs
ESLint設定。Flat Config形式でTypeScript、Playwright、スタイルルールを定義。

### pelicanconf.py
Pelican設定。プラグイン、テーマ、URL構造を定義。

### tsconfig.json
TypeScript設定。厳格な型チェック、ESNext対応、絶対パスエイリアス設定。

### devenv.nix
開発環境の定義。Python、Node.js、Git Hooksの設定を含む。自動フォーマット・リントを実行。

## 開発時の注意点

### 1. ファイル変更時の影響範囲

- **theme/src/**の変更 → Webpackビルドが必要
- **content/**の変更 → Pelicanビルドが必要
- **plugins/**の変更 → Pelican再起動が必要

### 2. 数式記事の取り扱い

- KaTeXを使用して数式をレンダリング
- `katex_math_render`プラグインが処理
- テストでは数式表示の確認が重要

### 3. 脚注機能

- Tippy.jsでツールチップ表示
- `add_footnote_tooltip.ts`で実装
- アクセシビリティに配慮した実装

### 4. テーマ機能

- ライト/ダークテーマ対応
- CSS変数とJavaScriptで切り替え
- レスポンシブデザイン対応

## テスト方針

PlaywrightでのE2Eテストを中心に、フロントエンドの動作確認を行います。

### テスト実行環境

- ベースURL: `http://localhost:8000`
- 対象ブラウザ: Chrome、Safari、Mobile Chrome
- 自動的にPython HTTPサーバーを起動
- タイムアウト: 10秒、リトライあり、スクリーンショット機能あり

## 新機能開発時のガイドライン

### 1. フロントエンド機能追加

1. `theme/src/`に適切なファイルを作成
2. TypeScriptの型定義を忘れずに
3. SCSSでスタイル定義
4. Playwrightテストを追加

### 2. Pelicanプラグイン開発

1. `plugins/custom/`に新しいプラグインを作成
2. `__init__.py`で適切に登録
3. docstringを含む適切な文書化
4. `pelicanconf.py`で有効化

### 3. コンテンツ追加

1. `content/articles/YYYY/`に記事を配置
2. reStructuredTextまたはMarkdown形式
3. 適切なメタデータ（日付、カテゴリ、タグ）
4. 画像は`content/asset/`に配置

## トラブルシューティング

### よくある問題

1. **ビルドエラー**: `make clean && make html`で再ビルド
2. **JavaScript エラー**: ブラウザのDevToolsでデバッグ
3. **CSS 表示崩れ**: `make js-build-dev`で再ビルド
4. **テスト失敗**: `npm run test -- --debug`でデバッグ

### デバッグ方法

```bash
# 詳細なログ出力
make DEBUG=1 html

# 開発サーバーでリアルタイム確認
make devserver

# テストのデバッグモード
npm run test -- --debug

# テストの UI モード（開発時に便利）
npm run test -- --ui
```

## 本番環境への配置

1. `make html`で本番ビルド
2. `output/`ディレクトリが配置対象
3. 静的ファイルホスティングサービスへアップロード
4. GitHub Actions等でCI/CD可能

## 開発環境の構築

### Dev Container対応

`.devcontainer/devcontainer.json`により、VS Code Dev Containerでの開発環境を提供。

### devenv使用

```bash
# 開発環境に入る
devenv shell

# 開発環境でサーバー起動
devenv shell
make devserver
```

---

**注意**: このドキュメントは開発効率化のためのガイドです。実際の開発では、既存のコード規約や設定を尊重し、一貫性を保つことを心がけてください。
