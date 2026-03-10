---
description: Review a GitHub pull request using a dedicated reviewer agent in a clean context. Use when you want an unbiased code review without the current conversation's context influencing the review.
allowed-tools: Read, "Bash(git branch --show-current)", "Bash(gh pr list *)", Glob, Grep, AskUserQuestion, TeamCreate, TeamDelete, Task, TaskCreate, TaskUpdate, TaskList, TaskGet, SendMessage, WebSearch
---

# PR レビュー

PR「$ARGUMENTS」を、クリーンなコンテキストの reviewer エージェントでレビューします。

## 手順

### 1. 対象 PR の特定

- `$ARGUMENTS` が指定されている場合: その PR 番号または URL を使用
- `$ARGUMENTS` が空の場合:
  1. `git branch --show-current` で現在のブランチ名を取得
  2. `gh pr list --head <branch-name> --json number,url --limit 1` で該当ブランチの PR を検索
  3. PR が見つかった場合はその PR をレビュー対象とする
  4. PR が見つからない場合はユーザーに PR 番号の指定を求める

### 2. チーム作成

```
TeamCreate({ team_name: "review-pr-{PR番号}", description: "PR #{PR番号} のレビュー" })
```

### 3. タスク作成

```
TaskCreate({
  subject: "PR #{PR番号} をレビュー",
  description: "PR #{PR番号} のコードレビューを実施し、GitHub の Review 機能でコメントを投稿する",
  activeForm: "PR #{PR番号} をレビュー中"
})
```

### 4. Reviewer エージェントの起動

`.claude/skills/autodev-review-pr/reviewer-spawn-prompt.md` を読み込み、spawn prompt として使用する。

spawn prompt 中の `{PR_NUMBER}` をレビュー対象の PR 番号に置換してから使用する。

```
Task({
  description: "Review PR #{PR番号}",
  prompt: "{reviewer-spawn-prompt.md の内容（{PR_NUMBER} を置換済み）}",
  subagent_type: "general-purpose",
  model: "opus",
  team_name: "review-pr-{PR番号}",
  name: "reviewer"
})
```

### 5. レビュー完了の確認

reviewer からのメッセージを待ち、レビュー結果を確認する。

### 6. チームの解散とシャットダウン

1. reviewer に `shutdown_request` を送信
2. reviewer のシャットダウンを待つ
3. `TeamDelete` でチームをクリーンアップ

**推奨アクションが APPROVE の場合**: reviewer のシャットダウンとチーム解散まで自動で行い、結果報告に進む。ユーザーの確認は不要。

**推奨アクションが REQUEST_CHANGES または COMMENT の場合**: reviewer をシャットダウンし、結果報告後にユーザーの判断を待つ。

### 7. 結果報告

reviewer のレビュー結果サマリーをユーザーに報告する。

## 注意事項

- reviewer は clean context で動作するため、現在の会話の文脈に影響されない公正なレビューが可能
- reviewer は steering docs（tech.md, structure.md 等）を自分で読み込んでレビューする
- 大きな PR でも reviewer が段階的にレビューする
