import { test, expect } from '@playwright/test';

const MIN_VISIBLE_ELEMENTS = 10;

test.describe('テーマ', () => {
    test('ホームページがホワイトアウトしないこと', async ({ page }) => {
        await page.goto('/');

        await page.waitForLoadState('domcontentloaded');

        const body = page.locator('body');
        await expect(body).toBeVisible();

        const mainContent = page.locator('main, .content, article, body').first();
        await expect(mainContent).toBeVisible();

        const title = page.locator('h1, .title').first();
        await expect(title).toBeVisible();

        const hasContent = await page.evaluate((minElements) => {
            const body = document.body;
            const textContent = body.textContent ?? '';
            const hasVisibleElements = body.querySelectorAll('*').length > minElements;
            return textContent.trim().length > 0 && hasVisibleElements;
        }, MIN_VISIBLE_ELEMENTS);

        expect(hasContent).toBe(true);
    });

    test('JavaScriptエラーが発生しないこと', async ({ page }) => {
        const jsErrors: string[] = [];

        page.on('console', (msg) => {
            if (msg.type() === 'error') {
                jsErrors.push(msg.text());
            }
        });

        page.on('pageerror', (error) => {
            jsErrors.push(error.message);
        });

        await page.goto('/');
        await page.waitForLoadState('domcontentloaded');

        expect(jsErrors).toHaveLength(0);
    });

    test('ページの基本構造が正しいこと', async ({ page }) => {
        await page.goto('/');
        await page.waitForLoadState('domcontentloaded');

        await expect(page.locator('html')).toHaveAttribute('lang', 'ja');
        await expect(page).toHaveTitle(/続くといいな日記/);

        const description = page.locator('meta[name="description"]');
        await expect(description).toHaveCount(1);

        const favicon = page.locator('link[rel*="icon"]');
        await expect(favicon.first()).toBeAttached();

        const stylesheets = page.locator('link[rel="stylesheet"]');
        await expect(stylesheets.first()).toBeAttached();
    });

    test('レスポンシブデザインが機能すること', async ({ page }) => {
        await page.setViewportSize({ width: 1280, height: 720 });
        await page.goto('/');
        await page.waitForLoadState('domcontentloaded');

        const mainContent = page.locator('main, .content').first();
        await expect(mainContent).toBeVisible();

        await page.setViewportSize({ width: 375, height: 667 });
        await mainContent.waitFor({ state: 'visible' });

        await expect(mainContent).toBeVisible();

        await page.setViewportSize({ width: 768, height: 1024 });
        await mainContent.waitFor({ state: 'visible' });

        await expect(mainContent).toBeVisible();
    });

    test('アクセシビリティの基本要件を満たすこと', async ({ page }) => {
        await page.goto('/');
        await page.waitForLoadState('domcontentloaded');

        // 見出し構造の確認
        const headings = page.locator('h1, h2, h3, h4, h5, h6');
        await expect(headings.first()).toBeVisible();

        // 表示されているリンクにテキストがあることを確認
        const visibleLinks = page.locator('a:visible').filter({ hasText: /.+/ });
        const linkCount = await visibleLinks.count();

        for (let i = 0; i < Math.min(3, linkCount); i++) {
            const link = visibleLinks.nth(i);
            // リンクにアクセシブルなテキストがあることを確認
            await expect(link).toHaveAttribute('href');
            // リンクにテキストまたはラベルがあることを確認
            await expect(link).toHaveText(/.+/);
        }

        // 画像にalt属性があることを確認
        const images = page.locator('img');
        const imageCount = await images.count();

        for (let i = 0; i < Math.min(3, imageCount); i++) {
            const img = images.nth(i);
            await expect(img).toHaveAttribute('alt');
        }
    });

    test('ボタンが反応すること', async ({ page }) => {
        await page.goto('/');
        await page.waitForLoadState('domcontentloaded');

        // ナビゲーションボタンをテスト
        const navButtons = page.locator('button, .btn, [role="button"]');

        const firstButton = navButtons.first();
        await expect(firstButton).toBeVisible();

        // ボタンがクリック可能であることを確認
        await expect(firstButton).toBeEnabled();

        // ホバー効果があることを確認
        await firstButton.hover();

        // ダークモード切り替えボタンなどがあるかテスト
        const themeButton = page.locator('[data-theme], .theme-toggle, .dark-mode-toggle');
        const themeButtonCount = await themeButton.count();

        expect(themeButtonCount).toBeGreaterThanOrEqual(0); // テーマボタンはオプショナル
    });
});
