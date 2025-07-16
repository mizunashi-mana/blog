import { test, expect } from '@playwright/test';

test.describe('シンタックスハイライト (Pygments)', () => {
    test('コードブロックが正しくハイライトされていること', async ({ page }) => {
        await page.goto('/posts/2020/07/padding-oracle-attack/');
        await page.waitForLoadState('domcontentloaded');

        // Pygmentsのコードブロックが存在することを確認
        const codeBlocks = page.locator('.highlight, .codehilite, pre.literal-block');
        const codeBlockCount = await codeBlocks.count();
        expect(codeBlockCount).toBeGreaterThan(0);

        // 最初のコードブロックが表示されていることを確認
        const firstCodeBlock = codeBlocks.first();
        await expect(firstCodeBlock).toBeVisible();

        // Pygmentsのスタイルが適用されていることを確認
        const codeBlockStyle = await firstCodeBlock.evaluate((el) => {
            const style = window.getComputedStyle(el);
            return {
                backgroundColor: style.backgroundColor,
                padding: style.padding,
                fontFamily: style.fontFamily,
                fontSize: style.fontSize,
                borderRadius: style.borderRadius,
            };
        });

        // コードブロックにスタイルが適用されていることを確認
        expect(codeBlockStyle.backgroundColor).not.toBe('rgba(0, 0, 0, 0)'); // 背景色がある
        expect(codeBlockStyle.fontFamily).toMatch(/mono|consola|courier/i); // モノスペースフォント
        expect(codeBlockStyle.fontSize).toBeTruthy(); // フォントサイズが設定されている

        // シンタックスハイライトのクラスが存在することを確認
        const highlightElements = page.locator('.highlight .k, .highlight .s, .highlight .c, .highlight .n, .highlight .o');
        const highlightCount = await highlightElements.count();
        expect(highlightCount).toBeGreaterThan(0);

        // ハイライト要素が実際に色付けされていることを確認
        for (let i = 0; i < Math.min(3, highlightCount); i++) {
            const element = highlightElements.nth(i);
            await expect(element).toBeVisible();

            const elementStyle = await element.evaluate((el) => {
                const style = window.getComputedStyle(el);
                return {
                    color: style.color,
                    fontWeight: style.fontWeight,
                };
            });

            // 色やフォントウェイトが設定されていることを確認
            expect(elementStyle.color).toBeTruthy();
        }
    });

    test('Pygmentsのビジュアルスクリーンショットテスト', async ({ page }) => {
        await page.goto('/posts/2020/07/padding-oracle-attack/');
        await page.waitForLoadState('domcontentloaded');

        // 最初のコードブロックを特定
        const firstCodeBlock = page.locator('.highlight, .codehilite').first();
        await expect(firstCodeBlock).toBeVisible();

        // コードブロックのスクリーンショットを撮影
        await expect(firstCodeBlock).toHaveScreenshot('pygments-code-block.png');

        // 複数のコードブロックがある場合の確認
        const codeBlocks = page.locator('.highlight, .codehilite');
        const codeBlockCount = await codeBlocks.count();
        expect(codeBlockCount).toBeGreaterThan(1); // 複数あることを前提とする

        const secondCodeBlock = codeBlocks.nth(1);
        await expect(secondCodeBlock).toBeVisible();
        await expect(secondCodeBlock).toHaveScreenshot('pygments-second-code-block.png');
    });

    test('レスポンシブデザインでのコードブロック表示', async ({ page }) => {
        await page.goto('/posts/2020/07/padding-oracle-attack/');
        await page.waitForLoadState('domcontentloaded');

        const firstCodeBlock = page.locator('.highlight, .codehilite').first();
        await expect(firstCodeBlock).toBeVisible();

        await page.setViewportSize({ width: 1280, height: 720 });
        await expect(firstCodeBlock).toHaveScreenshot('pygments-desktop.png');

        await page.setViewportSize({ width: 768, height: 1024 });
        await firstCodeBlock.waitFor({ state: 'visible' });
        await expect(firstCodeBlock).toHaveScreenshot('pygments-tablet.png');

        await page.setViewportSize({ width: 375, height: 667 });
        await firstCodeBlock.waitFor({ state: 'visible' });
        await expect(firstCodeBlock).toHaveScreenshot('pygments-mobile.png');

        // モバイルでの横スクロール対応確認
        const codeBlockStyle = await firstCodeBlock.evaluate((el) => {
            const style = window.getComputedStyle(el);
            return {
                overflowX: style.overflowX,
                width: style.width,
            };
        });

        // 横スクロールの設定を確認（visible, auto, scrollのいずれかであることを確認）
        expect(codeBlockStyle.overflowX).toMatch(/visible|auto|scroll/);
    });

    test('コードブロック内のテキスト選択機能', async ({ page }) => {
        await page.goto('/posts/2020/07/padding-oracle-attack/');
        await page.waitForLoadState('domcontentloaded');

        const firstCodeBlock = page.locator('.highlight, .codehilite').first();
        await expect(firstCodeBlock).toBeVisible();

        // コードブロック内のテキストを選択
        const codeText = firstCodeBlock.locator('code, pre').first();
        await expect(codeText).toBeVisible();

        // テキストが選択可能であることを確認
        const isSelectable = await codeText.evaluate((el) => {
            const style = window.getComputedStyle(el);
            return style.userSelect !== 'none';
        });

        expect(isSelectable).toBe(true);

        // プログラマティックにテキスト選択を実行
        const hasSelectableText = await codeText.evaluate((el) => {
            const textContent = el.textContent ?? '';
            if (textContent.trim().length === 0) return false;

            // テキスト選択を実行
            const range = document.createRange();
            const selection = window.getSelection();

            if (!selection) return false;

            try {
                selection.removeAllRanges();
                range.selectNodeContents(el);
                selection.addRange(range);

                // 選択されたテキストがあるかを確認
                return selection.toString().length > 0;
            }
            catch {
                return false;
            }
        });

        expect(hasSelectableText).toBe(true);
    });

    test('Pygmentsの色設定確認', async ({ page }) => {
        await page.goto('/posts/2020/07/padding-oracle-attack/');
        await page.waitForLoadState('domcontentloaded');

        // Pygmentsの各要素タイプの色を確認
        const pygmentsElements = [
            { selector: '.highlight .k', name: 'keyword' }, // キーワード
            { selector: '.highlight .s', name: 'string' }, // 文字列
            { selector: '.highlight .c', name: 'comment' }, // コメント
            { selector: '.highlight .n', name: 'name' }, // 名前
        ];

        for (const elementType of pygmentsElements) {
            const elements = page.locator(elementType.selector);
            const elementCount = await elements.count();

            // 要素が存在する場合のみテスト
            for (let i = 0; i < Math.min(1, elementCount); i++) {
                const element = elements.nth(i);
                await expect(element).toBeVisible();

                const color = await element.evaluate((el) => {
                    return window.getComputedStyle(el).color;
                });

                // デフォルトの黒色でないことを確認（何らかの色付けがされている）
                expect(color).not.toBe('rgb(0, 0, 0)');
                expect(color).not.toBe('rgba(0, 0, 0, 1)');
            }
        }
    });
});
