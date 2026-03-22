import { buildConfig } from '@mizunashi_mana/eslint-config-refined';

export default [
    ...buildConfig({
        ruleSets: ['common', 'node', 'playwright'],
        entrypointFiles: ['theme/src/index.ts', 'scripts/**/*.ts', 'plugins/custom/katex_math_render/katex_render.js'],
        stylistic: {
            indent: 4,
            semi: true,
            quotes: 'single',
        },
        playwrightFiles: ['tests/**/*'],
    }),
];
