---
description: Create a GitHub issue from an idea, bug report, or feature request. Use when reporting bugs, proposing new features, or documenting problems as issues.
allowed-tools: Read, "Bash(gh issue list *)", mcp__github__issue_write, AskUserQuestion
---

# GitHub Issue 作成

アイデアから GitHub Issue を作成します。

## 入力

ユーザーから以下の情報を取得します:

- **概要**: Issue のタイトルと説明
- **種類**: Bug / Feature / Problem
- **優先度**: 高 / 中 / 低

## 手順

1. **情報収集**:
   - ユーザーから Issue の内容を確認
   - 種類と優先度を決定

2. **重複確認**:
   - `gh issue list --search "<キーワード>" --json number,title,url` で既存 Issue との重複がないか確認

3. **ラベル決定**:
   - 種類に応じて:
     - Bug（不具合）: `bug`
     - Feature（機能要望）: `enhancement`
     - Problem（課題）: `problem`
   - 優先度に応じて: `priority: high`, `priority: medium`, `priority: low`

4. **本文確認**:
   - 作成する Issue のタイトルと本文をユーザーに提示
   - AskUserQuestion で「この内容で作成してよいか？」を確認
   - ユーザーが修正を希望する場合は内容を調整

5. **Issue 作成**:
   - GitHub MCP を使用して Issue を作成

6. **完了報告**:
   - 作成した Issue の URL をユーザーに報告

## Issue テンプレート

### Bug Report

```markdown
## 概要

{不具合の説明}

## 再現手順

1. ...
2. ...

## 期待される動作

{正常な動作}

## 実際の動作

{現在の問題}

## 追加情報

{環境情報など}
```

### Feature Request

```markdown
## 概要

{機能の説明}

## 背景・動機

{なぜこの機能が必要か}

## 提案する解決策

{実装のアイデア}

## 代替案

{他に検討した方法}
```

### Problem

```markdown
## 課題

{どのような問題・不便さがあるか}

## 影響

{この課題による影響}

## 発生状況

{どのような状況で発生するか}

## アイデア（任意）

{解決のヒントになりそうなアイデア}
```

## 注意事項

- 引数 `$ARGUMENTS` が提供されている場合は、それを初期情報として使用
- 情報が不足している場合は AskUserQuestion で確認
- 本文確認で修正が必要な場合は、ユーザーの指示に従って内容を調整してから再度確認
