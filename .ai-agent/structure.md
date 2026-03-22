# 続くといいな日記 ディレクトリ構成

## 全体構造

```
blog/
├── .ai-agent/                # AI エージェント向けドキュメント
│   ├── steering/             # 戦略的ガイドドキュメント
│   │   ├── market.md         # 市場分析・競合調査
│   │   ├── plan.md           # 実装計画・記事候補
│   │   ├── product.md        # プロダクトビジョン
│   │   ├── tech.md           # 技術アーキテクチャ
│   │   └── work.md           # 開発ワークフロー
│   ├── structure.md          # このファイル（ディレクトリ構成）
│   ├── projects/             # 長期プロジェクト
│   ├── tasks/                # 個別タスク
│   └── surveys/              # 技術調査
├── .claude/                  # Claude Code 設定
│   ├── settings.json         # Claude Code 設定
│   ├── settings.local.json   # Claude Code ローカル設定（gitignore）
│   └── skills/               # autodev スキル群
├── .github/                  # GitHub 関連
│   ├── actions/              # カスタム Actions
│   │   └── setup-devenv/     # devenv セットアップ
│   └── workflows/            # CI/CD ワークフロー
│       ├── build.yml         # ビルド検証
│       ├── deploy-gh-pages.yml # GitHub Pages デプロイ
│       ├── lint.yml          # リント
│       ├── test.yml          # Python テスト
│       └── test-e2e.yml      # E2E テスト
├── content/                  # コンテンツ
│   ├── articles/             # ブログ記事（年別ディレクトリ）
│   │   ├── 2019/ ~ 2026/    # 各年のディレクトリに rst/md 記事を配置
│   ├── asset/                # 画像・ファビコン等の静的アセット
│   └── pages/                # 固定ページ
├── docs/                     # 開発ドキュメント
│   └── CodingGuideline.md   # コーディング規約
├── plugins/                  # Pelican プラグイン
│   ├── custom/               # カスタムプラグイン
│   │   ├── autocorrect_filetime/  # Git コミット日時でファイル日時補正
│   │   └── katex_math_render/     # KaTeX 数式サーバーサイドレンダリング
│   └── official/             # 公式プラグイン（git submodule）
├── scripts/                  # ユーティリティスクリプト
│   ├── cc-hook-*.ts          # Claude Code フック（フォーマット、ツール制御）
│   ├── playwright.sh         # Playwright テスト実行ヘルパー
│   └── test-server.ts        # テスト用 HTTP サーバー
├── tests/                    # テスト
│   ├── features/             # Playwright E2E テスト
│   │   ├── assets/           # アセット関連テスト
│   │   ├── plugins/          # プラグイン関連テスト
│   │   └── theme/            # テーマ関連テスト
│   └── tools/                # ツール設定テスト
│       ├── eslint/           # ESLint 設定テスト
│       └── ruff/             # Ruff 設定テスト
├── theme/                    # テーマ
│   ├── content/              # Pelican テーマ出力
│   │   ├── static/           # ビルド済み静的ファイル（JS/CSS）
│   │   └── templates/        # Jinja2 テンプレート
│   └── src/                  # フロントエンドソースコード
│       ├── component/        # 機能コンポーネント（脚注ツールチップ、Sentry等）
│       ├── lib/              # 外部ライブラリラッパー（Font Awesome, KaTeX）
│       ├── style/            # SCSS スタイルファイル
│       ├── types/            # TypeScript 型定義
│       ├── util/             # ユーティリティ（デバイス検出、ポリフィル）
│       ├── index.ts          # メインエントリーポイント
│       └── main.ts           # アプリケーションメイン
├── output/                   # ビルド出力（静的サイト、gitignore）
├── CLAUDE.md                 # Claude Code 開発ガイド
├── Makefile                  # ビルドコマンド
├── devenv.nix                # Nix 開発環境定義
├── devenv.yaml               # devenv 設定
├── eslint.config.mjs         # ESLint 設定（Flat Config）
├── package.json              # Node.js 依存関係
├── pelicanconf.py            # Pelican 設定
├── playwright.config.mjs     # Playwright テスト設定
├── pyproject.toml            # Python 依存関係
├── ruff.toml                 # Ruff 設定
├── tsconfig.json             # TypeScript 設定
├── webpack.common.config.js  # Webpack 共通設定
├── webpack.dev.config.js     # Webpack 開発設定
└── webpack.prod.config.js    # Webpack 本番設定
```

## アーキテクチャパターン

### ビルドパイプライン

二段構成のビルドフロー:

1. **フロントエンド**: `theme/src/` (TypeScript/SCSS) → Webpack → `theme/content/static/` (JS/CSS)
2. **コンテンツ**: `content/` (rst/md) → Pelican + カスタムプラグイン → `output/` (HTML)

### プラグインアーキテクチャ

- `plugins/custom/`: プロジェクト固有のプラグイン（Pelican signals API で接続）
- `plugins/official/`: pelican-plugins リポジトリからの git submodule

### テスト構成

- `tests/features/`: Playwright E2E テスト（ブラウザ別: Chrome, Safari, Mobile Chrome）
- `tests/tools/`: ツール設定のスナップショットテスト（pytest）
- テストは `http://localhost:8000` で起動した Python HTTP サーバーに対して実行
