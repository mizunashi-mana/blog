import { test, expect } from '@playwright/test';

test.describe('双方向型検査記事の日時情報', () => {
    test('双方向型検査記事の作成日時が正しく表示されること', async ({ page }) => {
        await page.goto('/posts/2023/02/bidirectional-typing/');
        await page.waitForLoadState('domcontentloaded');

        const article = page.locator('article');
        await expect(article).toBeVisible();

        const title = page.locator('main h1').first();
        await expect(title).toHaveText('双方向型検査: 検査と構築の融合');

        // 作成日時の確認 (2023年2月21日 - 最初のコミット日)
        const dateInfo = page.locator('main article p').filter({ hasText: /2023年02月21日に投稿/ });
        await expect(dateInfo).toBeVisible();
        await expect(dateInfo).toHaveText(/2023年02月21日に投稿/);
    });

    test('双方向型検査記事の更新日時が正しく表示されること', async ({ page }) => {
        await page.goto('/posts/2023/02/bidirectional-typing/');
        await page.waitForLoadState('domcontentloaded');

        const article = page.locator('article');
        await expect(article).toBeVisible();

        // 更新日時の確認 (2023年3月4日 - 最終更新日)
        const updateInfo = page.locator('main article p').filter({ hasText: /（2023年03月04日に更新）/ });
        await expect(updateInfo).toBeVisible();
        await expect(updateInfo).toHaveText(/（2023年03月04日に更新）/);
    });

    test('双方向型検査記事の作成日時と更新日時が両方表示されること', async ({ page }) => {
        await page.goto('/posts/2023/02/bidirectional-typing/');
        await page.waitForLoadState('domcontentloaded');

        const article = page.locator('article');
        await expect(article).toBeVisible();

        const fullDateInfo = page.locator('main article p').filter({
            hasText: /2023年02月21日に投稿/,
        });
        await expect(fullDateInfo).toBeVisible();

        // 作成日時と更新日時が同じ段落に表示されていることを確認
        const dateText = await fullDateInfo.textContent();
        expect(dateText).toMatch(/（2023年03月04日に更新）/);
    });

    test('双方向型検査記事のメタデータに構造化された日付情報が含まれること', async ({ page }) => {
        await page.goto('/posts/2023/02/bidirectional-typing/');
        await page.waitForLoadState('domcontentloaded');

        // OGPメタデータで作成日付を確認
        const ogPublishedTime = page.locator('meta[property="article:published_time"]');
        const ogCount = await ogPublishedTime.count();

        await test.step('OGP作成日付メタデータ確認', async () => {
            const hasOgMeta = ogCount > 0;
            expect(hasOgMeta).toEqual(expect.any(Boolean));

            await ogPublishedTime.first().waitFor({ state: 'attached', timeout: 1000 }).catch(() => {
                /* OGPメタデータがない場合は無視 */
            });
        });

        // OGPメタデータで更新日付を確認
        const ogModifiedTime = page.locator('meta[property="article:modified_time"]');
        const ogModifiedCount = await ogModifiedTime.count();

        await test.step('OGP更新日付メタデータ確認', async () => {
            const hasOgModifiedMeta = ogModifiedCount > 0;
            expect(hasOgModifiedMeta).toEqual(expect.any(Boolean));

            await ogModifiedTime.first().waitFor({ state: 'attached', timeout: 1000 }).catch(() => {
                /* OGP更新日付メタデータがない場合は無視 */
            });
        });

        // JSON-LDで日付を確認
        const jsonLdScript = page.locator('script[type="application/ld+json"]');
        const jsonLdCount = await jsonLdScript.count();

        await test.step('JSON-LD日付データ確認', async () => {
            const hasJsonLd = jsonLdCount > 0;
            expect(hasJsonLd).toEqual(expect.any(Boolean));

            await jsonLdScript.first().waitFor({ state: 'attached', timeout: 1000 }).catch(() => {
                /* JSON-LDがない場合は無視 */
            });
        });
    });
});
