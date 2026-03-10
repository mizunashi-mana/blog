---
description: Update steering documents (product.md, tech.md, plan.md, structure.md, etc.) to reflect current project state. Use when documents have become outdated or after completing significant work.
allowed-tools: Read, Write, Edit, MultiEdit, "Bash(mkdir *)", "Bash(gh issue list *)", Glob, Grep
---

# Steering ドキュメント更新

プロジェクトの現状を確認し、steering ドキュメントを最新化します。

## 対象ドキュメント

| ファイル                        | 内容                                             |
| ------------------------------- | ------------------------------------------------ |
| `README.md`                     | プロジェクト概要、技術スタック、セットアップ手順 |
| `.ai-agent/steering/product.md` | プロダクト概要、機能一覧                         |
| `.ai-agent/steering/tech.md`    | 技術スタック、開発コマンド                       |
| `.ai-agent/steering/market.md`  | 市場分析、競合調査                               |
| `.ai-agent/steering/plan.md`    | 実装計画、フェーズ状態                           |
| `.ai-agent/steering/work.md`    | 作業の進め方                                     |
| `.ai-agent/structure.md`        | ディレクトリ構成                                 |

## 手順

### 1. 現状把握

プロジェクトの実態を確認する:

- `ls content/articles/` で記事ディレクトリ構成を確認
- `ls plugins/custom/` でカスタムプラグインを確認
- `ls theme/src/` でフロントエンドソース構成を確認
- `cat package.json | jq '.dependencies,.devDependencies'` で依存関係を確認
- `cat pyproject.toml` で Python 依存関係を確認

### 2. GitHub イシュー確認

GitHub イシューを確認し、未対応の課題や進行中のタスクを把握:

- `gh issue list --state open --json number,title,labels,url` でオープンイシューを取得
- 機能追加・バグ修正・改善などのラベルで分類
- plan.md のフェーズと関連するイシューを特定

確認ポイント:

- オープンイシューに対応する plan.md のタスクがあるか
- イシューがクローズ済みだが plan.md で完了としてマークされていないタスク
- イシューで議論された仕様変更が product.md に反映されているか

### 3. ドキュメント読み込み

各ドキュメントを読み込み、現状と比較:

- 未作成と書かれているが作成済みのもの
- 存在すると書かれているが実際にないもの
- 完了状態が実態と異なるもの
- コマンドが存在しない/変更されているもの

### 4. 陳腐化箇所の特定

ユーザーに以下の形式で報告:

```
## 陳腐化箇所

| ファイル | 箇所 | 現状 | ドキュメント記載 |
|---------|------|------|-----------------|
| tech.md | XX パッケージ | 作成済み | 「未作成」 |
| plan.md | XX 機能 | Issue #N はクローズ済み | 「進行中」 |
```

### 5. ユーザー確認

修正内容を提示し、承認を得る:

- 修正する項目一覧
- 修正しない項目があればその理由

### 6. 修正実行

承認後、各ファイルを編集:

- 事実に基づく修正のみ行う
- 推測で情報を追加しない
- 将来の予定は現状のままにする

### 7. コミット

```bash
git add README.md .ai-agent/
git commit -m "Update steering documents to reflect current project state"
```

## 確認ポイント

### README.md

- プロダクトの概要・コンセプトが伝わりやすいものになっているか
- 技術スタックが最新か
- セットアップ手順が動作するか

### tech.md

- パッケージ/モジュール状態が正しいか
- 開発コマンドが存在するか
- バージョン情報が正しいか

### plan.md

- フェーズの状態（完了/進行中/未着手）
- 実装順序が現状を反映しているか

### structure.md

- ディレクトリ構成が実態と一致するか
- 各ディレクトリの説明が正確か

### GitHub イシューとの整合性

- オープンイシューが plan.md に反映されているか
- 完了済みタスクに対応するイシューがクローズされているか
- 機能要求イシューが product.md の機能一覧と整合しているか

## 注意事項

- product.md は機能仕様なので、実装状態ではなく設計意図として扱う
- work.md は作業フローなので、変更は慎重に
- 将来実装予定の記載は削除しない（plan.md で状態管理）
