import { test, expect } from '@playwright/test';

const MIN_KATEX_ELEMENTS = 5;

test.describe('数式', () => {
    test('数式が埋め込まれた記事が正しく表示されること', async ({ page }) => {
    // F-ing modules記事に直接アクセス（数式を含む記事として確認済み）
        await page.goto('/posts/2024/11/fing-modules/');
        await page.waitForLoadState('domcontentloaded');

        // KaTeX要素が描画されていることを確認
        const katexElements = page.locator('.katex, .katex-display');
        await expect(katexElements.first()).toBeVisible();

        // 複数の数式要素が存在することを確認
        const katexCount = await katexElements.count();
        expect(katexCount).toBeGreaterThan(MIN_KATEX_ELEMENTS); // F-ing modules記事には多数の数式がある

        // インライン数式の確認
        const inlineMath = page.locator('.katex:not(.katex-display)');
        await expect(inlineMath.first()).toBeVisible();

        // 数式が実際にレンダリングされていることを確認（空でない）
        await expect(inlineMath.first()).not.toHaveText('');

        // ブロック数式の確認
        const displayMath = page.locator('.katex-display');
        await expect(displayMath.first()).toBeVisible();

        // ブロック数式がレンダリングされていることを確認
        await expect(displayMath.first()).not.toHaveText('');

        // KaTeX HTMLが適切にレンダリングされていることを確認
        const katexHtml = page.locator('.katex-html');
        await expect(katexHtml.first()).toBeVisible();

        // 記事タイトルが正しく表示されていることを確認
        const articleTitle = page.locator('h1#fing-modules');
        await expect(articleTitle).toContainText('F-ing modules');
    });
});
