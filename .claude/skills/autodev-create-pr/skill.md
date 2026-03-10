---
description: Create a GitHub pull request from the current branch's changes. Use when changes are ready for review and you want to open a PR.
allowed-tools: Read, Glob, "Bash(git status *)", "Bash(git log *)", "Bash(git diff *)", "Bash(git push *)", "Bash(git branch --show-current)", "Bash(gh pr view *)", mcp__github__create_pull_request, mcp__github__update_pull_request
---

# PR 作成

現在のブランチの変更内容から PR を作成します。

## 手順

1. **現在の状態を確認**:
   - `git status` で未コミットの変更がないか確認
   - `git log master..HEAD --oneline` で master からのコミット一覧を確認
   - `git diff master...HEAD --stat` で変更ファイルを確認

2. **リモートにプッシュ**:
   - ブランチがリモートにない場合は `git push -u origin <branch>` でプッシュ

3. **PR テンプレートを確認**:
   - `.github/PULL_REQUEST_TEMPLATE.md` があれば読み込む

4. **PR を作成**:
   - `mcp__github__create_pull_request` を使用
   - タイトル: 変更内容を簡潔に要約
   - ボディ: PR テンプレートに沿って記載
     - 目的: 変更の背景・目的
     - 変更概要: 主な変更点を箇条書き
   - 注意点：改行のエスケープは不要。PR 説明の改行がエスケープされていないか確認する

5. **PR の内容を確認**:
   - `gh pr view {pull_number} --json body` で作成された PR の本文を取得
   - 改行が `\n` のようにエスケープされたまま表示されていないか確認
   - 問題がある場合は `mcp__github__update_pull_request` で修正する

6. **PR URL を報告**:
   - 作成した PR の URL をユーザーに伝える

## 注意事項

- コミットが済んでいない変更がある場合は、先にコミットするか確認する
- master ブランチへの直接プッシュは避ける
- PR タイトルは日本語で簡潔に（50文字以内推奨）
