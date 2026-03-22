# 実装計画

## 現在のフェーズ

保守・機能追加フェーズ。2019 年から継続運用中。

## 直近の完了作業

- npm パッケージの依存関係更新 (PR #350)
- ESLint 設定を `@mizunashi_mana/eslint-config-refined` に移行 (PR #349)
- devenv 環境の更新 (PR #346, #348)
- React View Props Pattern 記事の追加 (PR #336)
- Ruff バージョン更新 (PR #335)
- GitHub Actions の依存関係更新 (Dependabot)

## 下書き記事（リモートブランチ）

以下のブランチに下書き記事が存在する。いずれも 2019〜2020 年に作成されたもので、完成度はまちまち。

| ブランチ | 記事タイトル | カテゴリ | 状態 |
|---|---|---|---|
| `add-finitary-projection` | CPO に関連する定義まとめ / Finitary Projection | 数学 | 記事本体あり (400+ 行) |
| `add-kotlin-coroutine` | Kotlin の coroutine の仕組み | プログラミング言語 | 記事本体あり (256 行) |
| `add-scala-collection-bench` | Scala コレクションの View の性能 | プログラミング | 記事本体あり (22 行、序盤のみ) |
| `caf-article` | Constant Applicative Forms | プログラミング言語 | タイトル・メタデータのみ (6 行) |
| `closure-conversion-article` | クロージャ変換とラムダリフティング | プログラミング言語 | 記事本体あり + LaTeX 図 |
| `rewrite-rule-article` | 書き換え規則とインライン展開 | プログラミング言語 | タイトル・メタデータのみ (8 行) |
| `support-gdpr-and-ccpa` | 個人情報提供の Opt-In / Opt-Out に対応する | 規範 | 参考リンク集 (27 行) |
| `unboxed-tuple` | GHC 拡張 UnboxedTuple と多値返却 | プログラミング言語 | 記事本体あり (17 行、序盤のみ) |

## 保守タスク

- Dependabot による npm / GitHub Actions の定期更新（継続中）
