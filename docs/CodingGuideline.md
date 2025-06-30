# コーディングガイドライン

このドキュメントは、ブログプロジェクトにおけるコーディング規約を定義しています。これらの規約は、ESLint、Stylelint、Ruffの設定に基づいており、一貫性のあるコードベースを維持するために重要です。

## TypeScript / JavaScript

### 基本設定

プロジェクトでは以下のESLint設定を使用しています：

- `@eslint/js` - JavaScript基本推奨設定
- `typescript-eslint` - TypeScript専用ルール（推奨・厳格・型チェック・スタイル）
- `@stylistic/eslint-plugin` - コードスタイル設定

### インデントとスペース

```typescript
// ✅ 正しい - 4スペースインデント
function processContent(content: string): string {
    if (content.length > 0) {
        return content.trim();
    }
    return '';
}

// ❌ 間違い - 2スペースやタブは使用しない
function processContent(content: string): string {
  if (content.length > 0) {
    return content.trim();
  }
  return '';
}
```

### セミコロンとクォート

```typescript
// ✅ 正しい - セミコロン必須、シングルクォート使用
import { tippy } from 'tippy.js';
const message = 'Hello, World!';
const config = { theme: 'light-border' };

// ❌ 間違い - セミコロンなし、ダブルクォート
import { tippy } from "tippy.js"
const message = "Hello, World!"
```

### 型定義

```typescript
// ✅ 正しい - 明示的な型定義
interface BlogPost {
    title: string;
    content: string;
    publishedAt: Date;
    tags: string[];
}

// 関数の型定義
function formatDate(date: Date): string {
    return date.toISOString().split('T')[0];
}

// ✅ 正しい - 型アサーションよりも型ガードを使用
function isBlogPost(obj: unknown): obj is BlogPost {
    return (
        typeof obj === 'object' &&
        obj !== null &&
        'title' in obj &&
        'content' in obj
    );
}

// ❌ 間違い - any型の使用を避ける
function processData(data: any): any {
    return data.something;
}
```

### 非同期処理

```typescript
// ✅ 正しい - async/await の使用
async function loadContent(): Promise<string> {
    try {
        const response = await fetch('/api/content');
        const data = await response.json();
        return data.content;
    } catch (error) {
        console.error('Failed to load content:', error);
        throw error;
    }
}

// ✅ 正しい - Promise の適切な型指定
function delay(ms: number): Promise<void> {
    return new Promise(resolve => setTimeout(resolve, ms));
}
```

### モジュールとインポート

```typescript
// ✅ 正しい - 明示的なインポート
import { tippy, Instance } from 'tippy.js';
import type { Props } from 'tippy.js';

// ✅ 正しい - 相対パスのインポート
import { formatDate } from '../util/date';
import { DeviceDetector } from './device';

// ❌ 間違い - デフォルトインポートの乱用
import * as everything from 'tippy.js';
```

### エラーハンドリング

```typescript
// ✅ 正しい - 具体的なエラータイプ
class ContentLoadError extends Error {
    constructor(message: string, public readonly url: string) {
        super(message);
        this.name = 'ContentLoadError';
    }
}

// ✅ 正しい - エラーの適切な処理
function processContent(content: string): string {
    if (!content) {
        throw new Error('Content cannot be empty');
    }
    
    try {
        return JSON.parse(content);
    } catch (error) {
        if (error instanceof SyntaxError) {
            throw new Error(`Invalid JSON format: ${error.message}`);
        }
        throw error;
    }
}
```

### Nullish Coalescing と Optional Chaining

```typescript
// ✅ 正しい - Nullish Coalescing Operator (??) を使用
// null/undefined の場合のみデフォルト値を使用
const textContent = element.textContent ?? '';
const config = userConfig ?? defaultConfig;

// ❌ 間違い - Logical OR (||) は falsy値 (0, '', false) でもデフォルト値を返す
const textContent = element.textContent || '';
const port = userPort || 3000; // userPort が 0 の場合、意図しない動作

// ✅ 正しい - Optional Chaining (?.) を使用
// プロパティが存在しない場合の安全なアクセス
if (element?.classList?.contains('active')) {
    // 処理
}

const linkText = link?.textContent?.trim();

// ❌ 間違い - 手動でのnullチェック
if (element && element.classList && element.classList.contains('active')) {
    // 処理
}

// ✅ 正しい - 複数の条件での使い分け
const hasAccessibleText = !!(linkText?.trim() ?? ariaLabel ?? title);

// メソッド呼び出しでの使用
element?.addEventListener?.('click', handler);

// 配列での使用
const firstItem = items?.[0];
const property = object?.[dynamicKey];
```

### ファイル構成

```
theme/src/
├── types/              # 型定義ファイル
│   ├── ext.css.d.ts
│   └── ext.scss.d.ts
├── component/          # 機能コンポーネント
│   ├── add_footnote_tooltip.ts
│   └── sentry_monitor.ts
├── lib/                # 外部ライブラリのラッパー
│   ├── fontawesome.ts
│   └── katex.ts
├── util/               # ユーティリティ関数
│   ├── device.ts
│   └── polyfills.ts
├── style/              # スタイルファイル
├── index.ts            # メインエントリーポイント
└── main.ts             # アプリケーションメイン
```

## SCSS

### 基本設定

プロジェクトでは以下のStylelint設定を使用しています：

- `stylelint-config-standard-scss` - SCSS標準設定
- `stylelint-prettier/recommended` - Prettier連携

### 変数の定義と使用

```scss
// ✅ 正しい - variables.scss で一元管理
$primary-color: #d9411e;
$secondary-color: #333;
$font-family-sans: 'Source Sans Pro', 'Roboto', sans-serif;
$font-family-mono: 'Source Code Pro', 'Consolas', monospace;

// テーマ対応変数
$body-bg: $white;
$body-bg-dark-theme: $grey;
$text-color: $dark-grey;
$text-color-dark-theme: $light-grey;
```

### セレクターの記述

```scss
// ✅ 正しい - BEM命名規則
.article {
    padding: 1rem;
    margin-bottom: 2rem;
    
    &__header {
        border-bottom: 1px solid $border-color;
        padding-bottom: 0.5rem;
    }
    
    &__title {
        font-size: 2rem;
        font-weight: 400;
        color: $text-color;
        line-height: 1.2;
        
        .dark-theme & {
            color: $text-color-dark-theme;
        }
    }
    
    &__content {
        margin-top: 1rem;
        
        p {
            margin-bottom: 1rem;
            line-height: 1.6;
        }
        
        code {
            font-family: $font-family-mono;
            background-color: $code-bg;
            padding: 0.2rem 0.4rem;
            border-radius: 3px;
        }
    }
    
    &--featured {
        border-left: 4px solid $primary-color;
        padding-left: 1.5rem;
    }
}

// ❌ 間違い - 深すぎるネスト
.article {
    .header {
        .title {
            .link {
                .text {
                    color: blue; // 5階層は深すぎる
                }
            }
        }
    }
}
```

### レスポンシブデザイン

```scss
// ✅ 正しい - モバイルファースト
.navigation {
    display: flex;
    flex-direction: column;
    
    // タブレット以上
    @media (min-width: 768px) {
        flex-direction: row;
        justify-content: space-between;
    }
    
    // デスクトップ
    @media (min-width: 1024px) {
        padding: 0 2rem;
    }
}

// ✅ 正しい - 変数を使用したブレークポイント
$breakpoint-tablet: 768px;
$breakpoint-desktop: 1024px;

.content {
    padding: 1rem;
    
    @media (min-width: $breakpoint-tablet) {
        padding: 2rem;
    }
}
```

### テーマ対応

```scss
// ✅ 正しい - ダークテーマ対応
.sidebar {
    background-color: $sidebar-bg;
    color: $sidebar-text-color;
    
    .dark-theme & {
        background-color: $sidebar-bg-dark-theme;
        color: $sidebar-text-color-dark-theme;
    }
}

// ✅ 正しい - CSS Custom Properties との併用
.button {
    --btn-bg: #{$btn-bg};
    --btn-text: #{$btn-text-color};
    
    background-color: var(--btn-bg);
    color: var(--btn-text);
    border: none;
    padding: 0.5rem 1rem;
    border-radius: 4px;
    cursor: pointer;
    
    &:hover {
        --btn-bg: #{$btn-hover-color};
    }
    
    .dark-theme & {
        --btn-bg: #{$btn-bg-dark-theme};
        --btn-text: #{$btn-text-color-dark-theme};
    }
}
```

### アニメーションとトランジション

```scss
// ✅ 正しい - パフォーマンスを考慮したアニメーション
.tooltip {
    opacity: 0;
    transform: translateY(-10px);
    transition: opacity 0.2s ease, transform 0.2s ease;
    will-change: opacity, transform;
    
    &--visible {
        opacity: 1;
        transform: translateY(0);
    }
}

// ✅ 正しい - アクセシビリティ考慮
@media (prefers-reduced-motion: reduce) {
    .tooltip {
        transition: none;
    }
}
```

## Python

### 基本設定

プロジェクトではRuffを使用しており、以下の設定が適用されています：

- Black準拠のフォーマット
- flake8、isort、pydocstyle等の統合
- 除外対象: `.venv`, `node_modules`, `output`, `plugins/official`

### インポート

```python
# ✅ 正しい - インポート順序（標準 → サードパーティ → ローカル）
from __future__ import unicode_literals
import json
import os
import subprocess
from typing import Dict, List, Optional, Union

from bs4 import BeautifulSoup
from pelican import contents, signals

from .git_process import get_git_commit_date
```

### 関数定義

```python
# ✅ 正しい - 型ヒントとdocstring
def process_math_content(content: str, config: Dict[str, str]) -> str:
    """数式コンテンツを処理してKaTeXでレンダリングする.
    
    Args:
        content: 処理対象のHTMLコンテンツ
        config: KaTeX設定辞書
        
    Returns:
        処理済みのHTMLコンテンツ
        
    Raises:
        ValueError: 無効なコンテンツが渡された場合
        subprocess.CalledProcessError: KaTeXレンダリングに失敗した場合
    """
    if not content.strip():
        raise ValueError("Content cannot be empty")
    
    soup = BeautifulSoup(content, 'html.parser')
    math_elements = soup.find_all('span', class_='math')
    
    for element in math_elements:
        try:
            rendered = render_katex(element.get_text(), config)
            element.replace_with(BeautifulSoup(rendered, 'html.parser'))
        except subprocess.CalledProcessError as e:
            logger.error(f"KaTeX rendering failed: {e}")
            raise
    
    return str(soup)
```

### クラス定義

```python
# ✅ 正しい - クラス定義
class KaTeXRenderer:
    """KaTeX数式レンダラー."""
    
    def __init__(self, config: Optional[Dict[str, str]] = None) -> None:
        """レンダラーを初期化する.
        
        Args:
            config: KaTeX設定辞書
        """
        self.config = config or {}
        self._process: Optional[subprocess.Popen] = None
    
    def render(self, expression: str) -> str:
        """数式をレンダリングする.
        
        Args:
            expression: 数式文字列
            
        Returns:
            レンダリング済みHTML
        """
        if not self._process:
            self._start_process()
        
        return self._render_expression(expression)
    
    def _start_process(self) -> None:
        """KaTeXプロセスを開始する."""
        self._process = subprocess.Popen(
            ["node", KATEX_RENDER_JS_PATH],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True
        )
    
    def _render_expression(self, expression: str) -> str:
        """数式を実際にレンダリングする."""
        # 実装...
        pass
```

### エラーハンドリング

```python
# ✅ 正しい - 具体的な例外処理
def load_article_metadata(filepath: str) -> Dict[str, Union[str, List[str]]]:
    """記事のメタデータを読み込む.
    
    Args:
        filepath: 記事ファイルのパス
        
    Returns:
        メタデータ辞書
        
    Raises:
        FileNotFoundError: ファイルが見つからない場合
        ValueError: メタデータの形式が不正な場合
    """
    try:
        with open(filepath, 'r', encoding='utf-8') as f:
            content = f.read()
    except FileNotFoundError:
        logger.error(f"Article file not found: {filepath}")
        raise
    except UnicodeDecodeError as e:
        logger.error(f"Failed to decode file {filepath}: {e}")
        raise ValueError(f"Invalid file encoding: {filepath}")
    
    try:
        metadata = parse_metadata(content)
    except Exception as e:
        logger.error(f"Failed to parse metadata in {filepath}: {e}")
        raise ValueError(f"Invalid metadata format in {filepath}")
    
    return metadata
```

### Pelicanプラグイン

```python
# ✅ 正しい - Pelicanプラグインの実装
def register():
    """プラグインを登録する."""
    signals.content_object_init.connect(process_content)
    signals.finalized.connect(cleanup_resources)


def process_content(content_object: contents.Content) -> None:
    """コンテンツオブジェクトを処理する.
    
    Args:
        content_object: Pelicanコンテンツオブジェクト
    """
    if not hasattr(content_object, '_content'):
        return
    
    # 数式処理
    if '<span class="math">' in content_object._content:
        try:
            content_object._content = process_math_content(
                content_object._content,
                content_object.settings.get('KATEX_CONFIG', {})
            )
        except Exception as e:
            logger.warning(f"Math processing failed for {content_object.source_path}: {e}")


def cleanup_resources() -> None:
    """リソースをクリーンアップする."""
    global proc
    if proc:
        proc.terminate()
        proc.wait()
        proc = None
```

## 命名規則

### TypeScript/JavaScript

- **変数・関数**: camelCase (`userName`, `calculateTotal`)
- **定数**: UPPER_SNAKE_CASE (`MAX_RETRY_COUNT`, `API_BASE_URL`)
- **クラス**: PascalCase (`ContentProcessor`, `DateFormatter`)
- **インターフェース**: PascalCase (`BlogPost`, `UserConfig`)
- **型エイリアス**: PascalCase (`StringOrNumber`, `ConfigOptions`)

### SCSS

- **変数**: kebab-case (`$primary-color`, `$font-size-large`)
- **クラス**: BEM記法 (`.article__title--featured`)
- **ミックスイン**: kebab-case (`@mixin button-style`)

### Python

- **変数・関数**: snake_case (`user_name`, `calculate_total`)
- **定数**: UPPER_SNAKE_CASE (`MAX_RETRY_COUNT`, `DEFAULT_CONFIG`)
- **クラス**: PascalCase (`ContentProcessor`, `DateFormatter`)
- **モジュール**: snake_case (`math_renderer.py`, `git_utils.py`)

## コメントとドキュメント

### TypeScript

```typescript
/**
 * ブログ記事を表すインターフェース
 */
interface BlogPost {
    /** 記事タイトル */
    title: string;
    /** 記事本文 */
    content: string;
    /** 公開日時 */
    publishedAt: Date;
}

/**
 * 日付を指定フォーマットで文字列に変換する
 * @param date - 変換対象の日付
 * @param format - フォーマット文字列
 * @returns フォーマット済み日付文字列
 */
function formatDate(date: Date, format: string): string {
    // 実装...
}
```

### Python

```python
def process_article(article_path: str, config: Dict[str, Any]) -> str:
    """記事を処理してHTMLを生成する.
    
    Args:
        article_path: 記事ファイルのパス
        config: 処理設定
        
    Returns:
        生成されたHTML文字列
        
    Raises:
        FileNotFoundError: 記事ファイルが見つからない場合
        ValueError: 設定が無効な場合
    """
    # 実装...
```

## E2Eテスト (Playwright)

### 基本原則

プロジェクトではPlaywrightを使用してE2Eテストを実装しています。以下の原則に従ってテストを記述してください。

#### 適用されているESLintルール

- `playwright/no-networkidle` - `networkidle` の使用を禁止
- `playwright/prefer-web-first-assertions` - Web-first assertions の使用を推奨
- `playwright/no-wait-for-selector` - `page.waitForSelector()` の使用を禁止
- `playwright/no-conditional-in-test` - テスト内での条件分岐を禁止
- `@typescript-eslint/no-empty-function` - 空の関数を禁止

### 待機処理

```typescript
// ✅ 正しい - domcontentloaded または load を使用
await page.waitForLoadState('domcontentloaded');
await page.waitForLoadState('load');

// ✅ 正しい - locator().waitFor() を使用
const tooltip = page.locator('[data-tippy-root] .tippy-box');
await tooltip.first().waitFor({ state: 'visible' });

const mainContent = page.locator('main, .content');
await mainContent.waitFor({ state: 'visible' });

// ✅ 正しい - 空の arrow function にはコメントを記述
await page.waitForFunction(() => {
    return document.body.classList.contains('dark-theme') || 
           document.body.classList.contains('light-theme');
}).catch(() => { /* テーマクラスがない場合は無視 */ });

// ❌ 間違い - networkidle の使用（非推奨）
await page.waitForLoadState('networkidle');

// ❌ 間違い - page.waitForSelector() の使用
await page.waitForSelector('main, .content', { state: 'visible' });

// ❌ 間違い - 固定時間の待機
await page.waitForTimeout(1000);

// ❌ 間違い - 空の arrow function
}).catch(() => {});
```

### マジックナンバーの回避

```typescript
// ✅ 正しい - 定数を定義して使用
const MIN_VISIBLE_ELEMENTS = 10;
const MIN_KATEX_ELEMENTS = 5;

test.describe('テーマ', () => {
    test('ページにコンテンツが存在すること', async ({ page }) => {
        const hasContent = await page.evaluate((minElements) => {
            const elementCount = document.querySelectorAll('*').length;
            return elementCount > minElements;
        }, MIN_VISIBLE_ELEMENTS);
        
        expect(hasContent).toBe(true);
    });
});

// ❌ 間違い - ハードコードされた数値
expect(katexCount).toBeGreaterThan(10);
expect(body.querySelectorAll('*').length > 10).toBe(true);
```

### アサーションとコンテンツ検証

```typescript
// ✅ 正しい - toHaveText() を使用してテキスト内容を検証
await expect(inlineMath.first()).not.toHaveText('');
await expect(link).toHaveText(/.+/); // 正規表現でパターンマッチ

// ✅ 正しい - 属性の存在確認
await expect(img).toHaveAttribute('alt');
await expect(link).toHaveAttribute('href');

// ❌ 間違い - textContent() の手動使用
const content = await element.textContent();
expect(content).toBeTruthy();
expect(content?.trim()).not.toBe('');
```

### 条件分岐の回避

```typescript
// ✅ 正しい - 直接的なアサーション
const elementCount = await elements.count();
for (let i = 0; i < Math.min(3, elementCount); i++) {
    const element = elements.nth(i);
    await expect(element).toBeVisible();
}

// ✅ 正しい - オプション要素の検証
const buttonCount = await buttons.count();
expect(buttonCount).toBeGreaterThanOrEqual(0);

// ❌ 間違い - 条件分岐を使った expect
if (elementCount > 0) {
    await expect(elements.first()).toBeVisible();
}

// ❌ 間違い - 複雑な条件ロジック
const hasAccessibleText = !!(linkText?.trim() ?? ariaLabel ?? title);
if (!hasAccessibleText) {
    expect(parentText?.trim()).toBeTruthy();
} else {
    expect(hasAccessibleText).toBe(true);
}
```

### 確実性とメンテナンス性

```typescript
// ✅ 正しい - 柔軟な条件設定
expect(katexCount).toBeGreaterThan(MIN_KATEX_ELEMENTS);

// ✅ 正しい - 存在確認の後に詳細テスト
const buttonCount = await navButtons.count();
if (buttonCount > 0) {
    const firstButton = navButtons.first();
    await expect(firstButton).toBeVisible();
    // 詳細なテストを実行
}

// ✅ 正しい - 冗長なアサーションを避ける
const tooltipContent = await tooltip.first().textContent();
expect(tooltipContent?.trim()).toBeTruthy(); // 1つのアサーションで十分

// ❌ 間違い - 同じことを複数回確認
expect(tooltipContent?.trim()).toBeTruthy();
expect(tooltipContent).toBeTruthy(); // 冗長
```

### page.evaluate の使用

```typescript
// ✅ 正しい - 外部変数を引数として渡す
const hasContent = await page.evaluate((minElements) => {
    const body = document.body;
    const textContent = body.textContent ?? '';
    const hasVisibleElements = body.querySelectorAll('*').length > minElements;
    return textContent.trim().length > 0 && hasVisibleElements;
}, MIN_VISIBLE_ELEMENTS);

// ❌ 間違い - 外部変数を直接参照（スコープエラーの原因）
const hasContent = await page.evaluate(() => {
    const hasVisibleElements = body.querySelectorAll('*').length > MIN_VISIBLE_ELEMENTS; // エラー
    return hasVisibleElements;
});
```

### テストの構造化

```typescript
// ✅ 正しい - 定数はファイルの先頭で定義
const MIN_VISIBLE_ELEMENTS = 10;
const MIN_KATEX_ELEMENTS = 5;

test.describe('テーマ', () => {
    test('基本機能テスト', async ({ page }) => {
        // 1. ページロード
        await page.goto('/');
        await page.waitForLoadState('networkidle');
        
        // 2. 基本要素の確認
        const mainContent = page.locator('main, .content');
        await mainContent.first().waitFor({ state: 'visible' });
        
        // 3. 詳細な検証
        // ...
    });
});
```

### ESLint ルール対応

```typescript
// ✅ 正しい - playwright/no-networkidle 対応
await page.waitForLoadState('domcontentloaded');
await page.waitForLoadState('load');

// ✅ 正しい - playwright/prefer-web-first-assertions 対応
await expect(element).toHaveText('expected text');
await expect(element).not.toHaveText('');

// ✅ 正しい - playwright/no-wait-for-selector 対応
const element = page.locator('.selector');
await element.waitFor({ state: 'visible' });

// ✅ 正しい - playwright/no-conditional-in-test 対応
// 条件分岐を避けて直接的なアサーションを使用
const elements = page.locator('.items');
await expect(elements.first()).toBeVisible();

// ✅ 正しい - @typescript-eslint/no-empty-function 対応
}).catch(() => { /* エラーを無視 */ });

// ❌ 間違い - ESLint ルール違反
await page.waitForLoadState('networkidle'); // playwright/no-networkidle
const text = await element.textContent(); // playwright/prefer-web-first-assertions
await page.waitForSelector('.selector'); // playwright/no-wait-for-selector
if (count > 0) { expect(...); } // playwright/no-conditional-in-test
}).catch(() => {}); // @typescript-eslint/no-empty-function
```

### エラーハンドリング

```typescript
// ✅ 正しい - オプショナルな要素の安全な処理
await page.locator('[data-tippy-root] .tippy-box').waitFor({ 
    state: 'hidden', 
    timeout: 1000 
}).catch(() => { /* タイムアウトは許容 */ });

// ✅ 正しい - 要素数の確認
const themeButtonCount = await themeButton.count();
expect(themeButtonCount).toBeGreaterThanOrEqual(0);
```

これらのガイドラインに従うことで、保守性が高く、一貫性のあるコードベースを維持できます。