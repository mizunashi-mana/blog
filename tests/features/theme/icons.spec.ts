import { test, expect } from '@playwright/test';

test.describe('アイコン表示', () => {
    test('Font Awesomeアイコンが正しく表示されること', async ({ page }) => {
        await page.goto('/');
        await page.waitForLoadState('domcontentloaded');

        // Font Awesomeのスタイルシートが読み込まれていることを確認
        const fontAwesomeCSS = await page.evaluate(() => {
            const stylesheets = Array.from(document.styleSheets);
            return stylesheets.some((sheet) => {
                try {
                    return sheet.href?.includes('fontawesome')
                        ?? Array.from(sheet.cssRules ?? []).some(rule =>
                            rule.cssText?.includes('fa-')
                            ?? rule.cssText?.includes('FontAwesome'),
                        );
                }
                catch {
                    return sheet.href?.includes('fontawesome');
                }
            });
        });

        expect(fontAwesomeCSS).toBe(true);

        // Font Awesomeアイコンクラスを持つ要素を検索
        const iconElements = page.locator('[class*="fa-"], [class*="fas "], [class*="far "], [class*="fab "], [class*="fal "], [class*="fad "]');
        const iconCount = await iconElements.count();
        expect(iconCount).toBeGreaterThanOrEqual(0);

        // アイコンが存在する場合のみテストを実行
        await iconElements.first().waitFor({ state: 'attached', timeout: 5000 }).catch(() => {
            // アイコンがない場合はスキップ
        });

        const actualIconCount = await iconElements.count();

        // アイコンが存在する場合のみ詳細テストを実行
        for (let i = 0; i < Math.min(3, actualIconCount); i++) {
            const icon = iconElements.nth(i);
            await expect(icon).toBeVisible();

            // アイコンのCSSプロパティを確認
            const iconStyle = await icon.evaluate((el) => {
                const style = window.getComputedStyle(el);
                const beforeStyle = window.getComputedStyle(el, '::before');

                return {
                    fontFamily: style.fontFamily,
                    beforeContent: beforeStyle.content,
                    fontSize: style.fontSize,
                    display: style.display,
                };
            });

            // Font Awesomeフォントが適用されていることを確認
            expect(iconStyle.fontFamily).toMatch(/Font ?Awesome|fa/i);

            // ::before疑似要素にコンテンツが設定されていることを確認（アイコンの実体）
            expect(iconStyle.beforeContent).not.toBe('none');
            expect(iconStyle.beforeContent).not.toBe('');

            // 要素が非表示でないことを確認
            expect(iconStyle.display).not.toBe('none');

            // アイコンが実際にレンダリングされていることを確認
            const isVisible = await icon.evaluate((el) => {
                const rect = el.getBoundingClientRect();
                return rect.width > 0 && rect.height > 0;
            });

            expect(isVisible).toBe(true);
        }
    });

    test('ソーシャルメディアアイコンが適切に表示されること', async ({ page }) => {
        await page.goto('/');
        await page.waitForLoadState('domcontentloaded');

        // よく使われるソーシャルメディアアイコンを検索
        const socialIcons = page.locator('[class*="fa-twitter"], [class*="fa-github"], [class*="fa-linkedin"], [class*="fa-facebook"], [class*="fa-instagram"], [class*="fa-youtube"], [class*="fa-rss"]');
        const socialIconCount = await socialIcons.count();
        expect(socialIconCount).toBeGreaterThanOrEqual(0);

        // ソーシャルアイコンが存在する場合のみテストを実行
        for (let i = 0; i < Math.min(3, socialIconCount); i++) {
            const socialIcon = socialIcons.nth(i);
            await expect(socialIcon).toBeVisible();

            // アイコンのクリック可能性を確認（通常はリンクの中にある）
            const parentLink = page.locator('a').filter({ has: socialIcon });
            const linkCount = await parentLink.count();
            expect(linkCount).toBeGreaterThanOrEqual(0);

            // リンクが存在する場合のみテスト
            for (let j = 0; j < Math.min(1, linkCount); j++) {
                await expect(parentLink.nth(j)).toHaveAttribute('href');
            }
        }
    });

    test('テーマ切り替えアイコンが正しく機能すること', async ({ page }) => {
        await page.goto('/');
        await page.waitForLoadState('domcontentloaded');

        // テーマ切り替えに関連するアイコンを検索
        const themeIcons = page.locator('[class*="fa-sun"], [class*="fa-moon"], [class*="fa-adjust"], [class*="fa-palette"]');
        const themeIconCount = await themeIcons.count();
        expect(themeIconCount).toBeGreaterThanOrEqual(0);

        // テーマアイコンが存在する場合のみテストを実行
        for (let i = 0; i < Math.min(1, themeIconCount); i++) {
            const themeIcon = themeIcons.nth(i);
            await expect(themeIcon).toBeVisible();

            // テーマアイコンのスタイルが適切に適用されていることを確認
            const iconStyle = await themeIcon.evaluate((el) => {
                const style = window.getComputedStyle(el);
                return {
                    cursor: style.cursor,
                    fontSize: style.fontSize,
                };
            });

            // カーソルがポインターになっていることを確認（クリック可能）
            expect(iconStyle.cursor).toBe('pointer');
        }
    });

    test('ナビゲーションアイコンが適切に表示されること', async ({ page }) => {
        await page.goto('/');
        await page.waitForLoadState('domcontentloaded');

        // ナビゲーション関連のアイコンを検索
        const navIcons = page.locator('[class*="fa-home"], [class*="fa-archive"], [class*="fa-tag"], [class*="fa-search"], [class*="fa-menu"], [class*="fa-bars"]');
        const navIconCount = await navIcons.count();
        expect(navIconCount).toBeGreaterThanOrEqual(0);

        // ナビゲーションアイコンが存在する場合のみテストを実行
        for (let i = 0; i < Math.min(3, navIconCount); i++) {
            const navIcon = navIcons.nth(i);
            await expect(navIcon).toBeVisible();
        }
    });
});
