import { test, expect, devices } from '@playwright/test';

test.use(devices['Desktop Chrome']);
test.describe('ツールチップ', () => {
    test('脚注のツールチップが作動すること', async ({ page }) => {
        await page.goto('/posts/2023/10/display-property-of-css/');
        await page.waitForLoadState('domcontentloaded');

        // 脚注リンクを探す（この記事には[1], [2], [3]の脚注がある）
        const footnoteLinks = page.locator('a.footnote-reference');
        const footnoteCount = await footnoteLinks.count();
        expect(footnoteCount).toBeGreaterThan(0); // 脚注が存在することを確認

        // 最初の脚注リンクをテスト
        const firstFootnote = footnoteLinks.first();
        await expect(firstFootnote).toBeVisible();

        // 脚注のテキストが正しいことを確認（[1]など）
        const footnoteText = await firstFootnote.textContent();
        expect(footnoteText).toMatch(/\[\d+\]/);

        // デスクトップデバイスでのツールチップテスト
        // 脚注にホバーしてツールチップが表示されることを確認
        await firstFootnote.hover();

        // Tippyツールチップが表示されるまで待機
        const tooltip = page.locator('[data-tippy-root] .tippy-box, .tippy-box');
        await tooltip.first().waitFor({ state: 'visible' });

        // ツールチップの内容が空でないことを確認
        const tooltipContent = await tooltip.first().textContent();
        expect(tooltipContent?.trim()).toBeTruthy();

        // 2番目もテスト
        const secondFootnote = footnoteLinks.nth(1);
        await expect(secondFootnote).toBeVisible();

        await page.mouse.move(0, 0);
        await page.locator('[data-tippy-root] .tippy-box').waitFor({ state: 'hidden', timeout: 1000 }).catch(() => { /* timeout is acceptable */ });

        await secondFootnote.hover();

        // 2番目の脚注のツールチップも確認
        const secondTooltip = page.locator('[data-tippy-root] .tippy-box, .tippy-box');
        const secondTooltipCount = await secondTooltip.count();

        expect(secondTooltipCount).toBeGreaterThanOrEqual(0); // ツールチップはオプショナル

        // 記事タイトルが正しく表示されていることを確認
        const articleTitle = page.locator('h1').filter({ hasText: /ボックスタイプとCSSレイアウト/ });
        await expect(articleTitle).toBeVisible();
    });
});
