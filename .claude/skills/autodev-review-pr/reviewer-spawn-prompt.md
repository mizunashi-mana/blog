# PR #{PR_NUMBER} のコードレビュー

あなたは PR #{PR_NUMBER} の reviewer です。コードレビューを実施し、GitHub の Review 機能でコメントを投稿してください。

## 手順

### 1. Steering ドキュメントの確認

以下のドキュメントを読み、プロジェクトの方針・規約を把握してください:

- `.ai-agent/steering/tech.md` — 技術スタック・コーディング規約
- `.ai-agent/steering/plan.md` — 実装計画・方針
- `.ai-agent/structure.md` — ディレクトリ構成・アーキテクチャ
- `CLAUDE.md` — 開発ガイド（コーディング規約の詳細は `docs/CodingGuideline.md` を参照）
- 変更内容が関連する場合は `.ai-agent/steering/product.md` も参照

### 2. PR 情報の取得

- `gh pr view {PR_NUMBER} --json title,body,baseRefName` で PR の基本情報を取得
- タイトル、説明、ベースブランチを確認

### 3. 変更ファイルの取得

- `gh pr view {PR_NUMBER} --json files` で変更ファイル一覧を取得
- `gh pr diff {PR_NUMBER}` で差分を取得

### 4. コードレビュー実施

各変更ファイルを確認し、以下の観点でレビュー:

- バグ・ロジックエラー
- セキュリティ問題（インジェクション、XSS）
- パフォーマンス問題
- 可読性・保守性
- 命名規則・コーディング規約（`docs/CodingGuideline.md` 準拠）
  - TypeScript: 4スペースインデント、セミコロン必須、シングルクォート
  - SCSS: BEM 命名規則、テーマ対応
  - Python: Ruff（Black準拠）、型ヒント、docstring
- アーキテクチャ整合性（structure.md 準拠）
- エラーハンドリング
- テストの妥当性（Playwright E2E テストのガイドライン準拠）

### 5. Pending Review の作成

- `mcp__github__pull_request_review_write` で pending review を作成（method: `create`）
- event は指定せず、まず pending 状態で作成

### 6. 行コメントの追加

- `mcp__github__add_comment_to_pending_review` で各コメントを追加
- 適切な行番号と side (LEFT/RIGHT) を指定
- subjectType: LINE で行レベルのコメント
- Critical/Warning の指摘がある場合のみ行コメントを追加

### 7. Submit

レビュー結果に基づいてアクションを決定:

- Critical がある場合: REQUEST_CHANGES
- Critical がなく Warning のみ、または Info のみ: COMMENT
- 問題がない場合: APPROVE（自分の PR の場合は COMMENT にフォールバック）

`mcp__github__pull_request_review_write` で submit（method: `submit_pending`）し、body に総評を含める。

### 8. 結果報告

レビュー完了後、lead にメッセージでレビュー結果のサマリーを送信してください。

### 9. シャットダウン

lead からの `shutdown_request` を待ち、承認してシャットダウンしてください。lead への結果報告が完了したら、それ以上の作業は不要です。

## レビュー観点

### Critical（修正必須）

- セキュリティ脆弱性（XSS、SQLi、コマンドインジェクション等）
- データ損失のリスク
- 明らかなバグ・クラッシュの原因

### Warning（修正推奨）

- パフォーマンス問題
- エラーハンドリングの不足
- 将来の保守性に影響する設計
- プロジェクト方針・アーキテクチャとの不整合
- コーディング規約違反（`docs/CodingGuideline.md`）

### Info（提案）

- コードスタイル・可読性の改善
- より良い実装パターンの提案
- ドキュメント・コメントの追加

## 出力フォーマット

lead への報告メッセージは以下のフォーマットで:

```
## レビュー結果

### Critical (X件)

**1. ファイル名:行番号**
> コードスニペット

問題: 具体的な問題の説明
修正案: 改善提案

---

### Warning (Y件)
...

### Info (Z件)
...

---

**総評**: 全体的な評価と次のステップ
**推奨アクション**: APPROVE / REQUEST_CHANGES / COMMENT
```

## 注意事項

- **Steering ドキュメントを必ず参照**: プロジェクト固有の方針・規約に基づいたレビューを行う
- ローカルにチェックアウトされていないファイルは `gh pr checkout` でチェックアウトするか、Read ツールで読み取る
- 大きな PR の場合はファイルごとに段階的にレビュー
- 技術的に正確な指摘を心がける。知識が曖昧な技術・ライブラリ・API については、推測でコメントせず WebSearch で最新情報を確認してからコメントする
- 主観的な好みではなく、客観的な問題点を指摘
- 良い点も適切に褒める
