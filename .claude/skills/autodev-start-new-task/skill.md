---
description: Start a new implementation task with branch, README, and structured workflow. Use when beginning a feature, bug fix, or improvement that takes a day to a few days.
allowed-tools: Read, Write, Edit, MultiEdit, Update, WebSearch, WebFetch
---

# 新規タスク開始

新しいタスク「$ARGUMENTS」を開始します。

## 手順

### 1. トリアージ

`$ARGUMENTS` の内容を分析し、このスキル（start-new-task）で扱うべきタスクかを判断する。

**判断基準:**

| 条件                                         | ルーティング先               | 例                                                        |
| -------------------------------------------- | ---------------------------- | --------------------------------------------------------- |
| 要件が曖昧・方向性の整理が必要               | `/autodev-discussion`        | 「開発フローを改善したい」「〇〇をどうにかしたい」        |
| 技術的な不確実性が高い・調査が先に必要       | `/autodev-start-new-survey`  | 「CI を高速化する方法を検討」「○○ライブラリの比較」       |
| 複数タスクへの分解が必要（数週間以上の規模） | `/autodev-start-new-project` | 「認証システムを全面リニューアル」「新機能Xの設計〜実装」 |
| 数時間〜半日で完了する具体的な実装タスク     | **そのまま続行**             | 「バリデーション追加」「設定ファイルの修正」              |

**そのまま続行する場合の目安:**

- ゴールが明確で、完了条件を具体的に書ける
- 技術的なアプローチが概ね見えている（大きな調査が不要）
- 1ブランチ・1PR で完結する規模
- 数時間〜半日で完了する見込み

**ルーティングする場合:**

- ユーザーに判断理由と推奨スキルを提示し、確認を取る
- タスクディレクトリ（手順 2〜3）が既に作成されている場合は、README の作業ログにトリアージ結果を記録する（例: 「トリアージの結果、`/autodev-discussion` にルーティング。理由: 要件が曖昧で方向性の整理が必要」）
- ユーザーが承認したら、推奨スキルに切り替える

### 2. タスク名の決定

- `$ARGUMENTS` の内容から適切なタスク名（英語、kebab-case）を考える
- 簡潔で内容が分かるタスク名にする

### 3. タスクディレクトリ作成

`.ai-agent/tasks/YYYYMMDD-{タスク名}/README.md` を作成（YYYYMMDD は今日の日付）

### 4. README.md に以下を記載

- 目的・ゴール
- 実装方針
- 完了条件
- 作業ログ（空欄で開始）

### 5. 関連ドキュメント確認

- `.ai-agent/steering/plan.md` で該当フェーズを確認
- `.ai-agent/steering/tech.md` で技術スタックを確認
- `.ai-agent/structure.md` でディレクトリ構成を確認

### 6. ユーザーに方針を提示して確認を取る

### 7. ブランチ作成（ユーザー確認後）

- `git checkout -b {タスク名}` でブランチを作成
- そのままブランチ作成することで、タスク内容を引き継ぐ。master の pull は後で良い

### 8. TodoWrite でタスクを細分化

### 9. 実装開始

## 実装中の注意

- 各ステップで動作確認を行う
- 動作確認コマンド:
  - `lint-all` - 全ファイルリント
  - `test-all` - 全テスト実行（pytest + Playwright）
  - `make js-build-dev` - フロントエンドビルド確認
  - `make devserver` - ブラウザでの目視確認
- 必要に応じてユーザーにフィードバックをもらう

## 完了時

- タスクの README に、完了条件をチェック
- タスクの README に、作業ログに結果を記載
- PR を作成する
  - `/autodev-create-pr` を使用する
- ユーザーに完了報告
