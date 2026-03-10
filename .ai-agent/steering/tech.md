# 技術アーキテクチャ

## 技術スタック

### フロントエンド

- **TypeScript** - メインエントリー: `theme/src/index.ts`
- **SCSS** - スタイル記述（BEM 命名規則）
- **Webpack** - モジュールバンドラー（dev/prod 設定分離）
- **KaTeX** - 数式レンダリング
- **Tippy.js** - 脚注ツールチップ
- **Font Awesome** - アイコン
- **Sentry** - エラー監視
- **core-js** - ポリフィル

### バックエンド/ビルド

- **Pelican** (Python) - 静的サイトジェネレーター
- **Python 3.13+** - プラグイン開発
- **uv** - Python 依存関係管理
- **devenv** (Nix) - 開発環境管理

### 品質管理

- **ESLint** + **Prettier** - TypeScript/JavaScript
- **Stylelint** - SCSS
- **Ruff** - Python
- **Playwright** - E2E テスト（Chrome, Safari, Mobile Chrome）
- **pytest** - Python ユニットテスト
- **Git Hooks** - devenv.nix による自動フォーマット・リント（commit 時）

### CI/CD

- **GitHub Actions** - ビルド、リント、テスト、デプロイ
  - `build.yml` - ビルド検証
  - `lint.yml` - リント
  - `test.yml` - Python テスト
  - `test-e2e.yml` - E2E テスト
  - `deploy-gh-pages.yml` - GitHub Pages デプロイ

## アーキテクチャ概要

### ビルドフロー

```
content/ (rst/md) ──→ Pelican ──→ output/ (HTML)
theme/src/ (ts/scss) ──→ Webpack ──→ theme/content/static/ (js/css)
```

Pelican がコンテンツを HTML に変換し、Webpack がフロントエンドアセットをバンドルする二段構成。

### カスタムプラグイン

- **katex_math_render** - 数式を KaTeX でサーバーサイドレンダリング
- **autocorrect_filetime** - Git コミット日時でファイル日時を補正

### テスト戦略

- **E2E テスト** (Playwright): テーマ表示、数式レンダリング、アセット読み込み等のフロントエンド動作検証
- **ユニットテスト** (pytest): プラグインのスナップショットテスト
- ベース URL: `http://localhost:8000`

## 開発環境

### セットアップ

```bash
git clone --recurse-submodules https://github.com/mizunashi-mana/blog.git
cd blog
devenv shell echo   # 開発環境初期化
install-all          # 依存関係インストール (uv sync && npm install)
```

### 主要コマンド

- `make devserver` - 開発サーバー起動
- `make html` - 本番ビルド
- `make js-build-dev` - フロントエンド開発ビルド
- `make js-build` - フロントエンド本番ビルド
- `lint-all` - 全ファイルリント
- `test-all` - 全テスト実行
