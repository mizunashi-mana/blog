import path from 'node:path';
import { defineConfig, devices } from '@playwright/test';

export default defineConfig({
    testDir: './tests/features',
    fullyParallel: true,
    forbidOnly: !!process.env.CI,
    retries: process.env.CI ? 2 : 1,
    workers: process.env.CI ? 1 : undefined,
    reporter: [
        ['html', { open: 'never' }],
    ],
    timeout: 10000,
    use: {
        baseURL: 'http://localhost:8000',
        trace: 'on-first-retry',
        screenshot: 'only-on-failure',
        actionTimeout: 10000,
    },

    projects: [
        {
            name: 'chromium',
            use: { ...devices['Desktop Chrome'] },
        },
        {
            name: 'webkit',
            use: { ...devices['Desktop Safari'] },
        },
        {
            name: 'Mobile Chrome',
            use: { ...devices['Pixel 5'] },
        },
    ],

    webServer: [
        {
            command: 'npx tsx ./scripts/test-server.ts',
            port: 8000,
            reuseExistingServer: !process.env.CI,
        },
    ],

    expect: {
        toHaveScreenshot: {
            stylePath: path.join(import.meta.dirname, 'tests/features/assets/for-snapshot-tests.css'),
            maxDiffPixelRatio: 0.01,
        },
    },
});
