# ESLint 設定を @mizunashi_mana/eslint-config-refined に移行

## 目的・ゴール

手動で構成している ESLint 設定を `@mizunashi_mana/eslint-config-refined` パッケージに置き換え、設定の標準化・簡素化を行う。

## 実装方針

1. ESLint を v9 → v10 にアップグレード
2. `@mizunashi_mana/eslint-config-refined` をインストール
3. `eslint.config.mjs` を `buildConfig()` ベースに書き換え
4. パッケージに内包されている devDependencies を整理（不要なものを削除）
5. lint を実行し、ルール差分を修正
6. CI が通ることを確認

## 完了条件

- [x] `@mizunashi_mana/eslint-config-refined` で ESLint 設定が動作する
- [x] 既存コードが新設定で lint エラーなし（または意図的な disable あり）
- [x] 不要な devDependencies が削除されている
- [ ] CI（lint, build, test）が通る

## 作業ログ

### 2026-03-22

- ESLint v9 → v10、`@mizunashi_mana/eslint-config-refined` v0.2.0 をインストール
- 9 個の devDependencies を削除（パッケージに内包）
- `eslint.config.mjs` を `buildConfig()` ベースに書き換え
  - `ruleSets: ['common', 'node', 'playwright']`
  - `entrypointFiles: ['theme/src/index.ts', 'scripts/**/*.ts']`
  - `stylistic: { indent: 4, semi: true, quotes: 'single' }`
- 新ルールへの対応:
  - `require-unicode-regexp`: 全正規表現に `v` フラグ追加
  - `no-plusplus`: `i++` → `i += 1`
  - `prefer-named-capture-group`: 非キャプチャグループに変換
  - `n/no-unsupported-features/node-builtins`: `engines.node` を `>=22.16.0` に設定
  - `@typescript-eslint/*`: ポリフィルファイルに eslint-disable 追加
- `tsconfig.json` の `target` を `ES2022` → `ES2024` に更新（`v` フラグサポート）
- `test-server.ts` を `import.meta.dirname` に簡略化
- lint エラー 0、Webpack ビルド成功を確認
