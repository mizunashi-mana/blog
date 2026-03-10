---
description: Switch to the default branch after a PR is merged, pull latest changes, and run compaction. Use after merging or having a PR merged to start fresh.
allowed-tools: "Bash(git checkout *)", "Bash(git pull *)", "Bash(git pull)", "Bash(git branch *)", "Bash(git rev-parse *)", "Bash(git remote *)", "Bash(git symbolic-ref *)", "Bash(git status *)", "Bash(gh pr list *)", mcp__github__merge_pull_request
---

# デフォルトブランチへの切り替え

PR のマージ後にデフォルトブランチへ切り替え、最新の変更を取り込み、compaction を実行して次の作業に備えます。

## 手順

### 1. PR のマージ状況を確認

現在のブランチに関連する PR があるかを確認する。

- `git remote get-url origin` でリポジトリの owner/repo を取得する
- `git branch --show-current` で現在のブランチ名を取得する
- `gh pr list --head <branch-name> --json number,state,url --limit 1` で現在のブランチの PR を検索する

結果に応じて分岐:

- **PR がマージ済み**: 次のステップに進む
- **PR がオープン**: `mcp__github__merge_pull_request` でマージコミット方式（`merge_method: "merge"`）でマージする。ユーザーへの確認は不要
  - CI がまだ通過してない場合や、レビューコメントが解決できていない場合にマージできない。その場合、レビューコメントが全て解決できるか確認し、CIが通るまで待つ
  - レビューコメントに未解決の問題がある場合は、ユーザーに確認する
- **PR が存在しない**: そのまま次のステップに進む

### 2. デフォルトブランチの特定

```bash
git symbolic-ref refs/remotes/origin/HEAD | sed 's@^refs/remotes/origin/@@'
```

上記でデフォルトブランチ名（例: `main`, `master`）を取得する。

### 3. 現在のブランチを確認

```bash
git branch --show-current
```

既にデフォルトブランチにいる場合は、切り替えをスキップして pull に進む。

### 4. デフォルトブランチに切り替え

```bash
git checkout <デフォルトブランチ>
```

### 5. 最新の変更を取り込み

```bash
git pull
```

### 6. compaction の実行

`/compact` の実行をユーザーに促す

## 注意事項

- 未コミットの変更がある場合は、切り替え前にユーザーに警告する
- pull に失敗した場合はエラー内容をユーザーに報告する
