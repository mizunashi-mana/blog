import eslintJs from '@eslint/js';
import pluginEslintComments from '@eslint-community/eslint-plugin-eslint-comments/configs';
import stylistic from '@stylistic/eslint-plugin';
import { importX } from 'eslint-plugin-import-x';
import eslintPluginPlaywright from 'eslint-plugin-playwright';
import pluginUnusedImports from 'eslint-plugin-unused-imports';
import globals from 'globals';
import typescriptEslint from 'typescript-eslint';

export default [
    {
        linterOptions: {
            reportUnusedDisableDirectives: 'error',
        },
    },
    {
        files: ['**/*.{js,mjs}'],
        languageOptions: {
            globals: {
                ...globals.node,
            },
            parserOptions: {
                projectService: false,
            },
        },
    },
    {
        files: ['**/*.{ts,tsx}', '.*/**/*.{ts,tsx}'],
        languageOptions: {
            parserOptions: {
                projectService: true,
            },
        },
    },
    {
        files: ['theme/src/**/*', 'tests/**/*', 'scripts/**/*'],
        languageOptions: {
            globals: {
                ...globals.commonjs,
                ...globals.browser,
            },
            parser: typescriptEslint.parser,
            parserOptions: {
                projectService: true,
            },
        },
    },
    eslintJs.configs.recommended,
    stylistic.configs.customize({
        indent: 4,
        semi: true,
    }),
    importX.flatConfigs.recommended,
    ...typescriptEslint.config(
        typescriptEslint.configs.recommended,
        typescriptEslint.configs.strict,
        typescriptEslint.configs.recommendedTypeChecked,
        typescriptEslint.configs.stylisticTypeChecked,
        {
            files: ['**/*.{js,mjs}'],
            extends: [typescriptEslint.configs.disableTypeChecked],
        },

        {
            files: ['**/*.{ts,tsx}', '.*/**/*.{ts,tsx}'],
            extends: [importX.flatConfigs.typescript],
        },
    ),
    pluginEslintComments.recommended,
    {
        plugins: {
            'unused-imports': pluginUnusedImports,
        },
    },
    {
        rules: {
            '@eslint-community/eslint-comments/disable-enable-pair': ['error', { allowWholeFile: true }],
            '@eslint-community/eslint-comments/no-aggregating-enable': 'error',
            '@eslint-community/eslint-comments/no-duplicate-disable': 'error',
            '@eslint-community/eslint-comments/no-unlimited-disable': 'error',
            '@eslint-community/eslint-comments/no-unused-enable': 'error',
            '@eslint-community/eslint-comments/require-description': [
                'error',
                {
                    ignore: ['eslint-enable', 'eslint-env'],
                },
            ],
            'import-x/export': 'error',
            'import-x/first': 'error',
            'import-x/no-absolute-path': ['error', { esmodule: true, commonjs: true, amd: false }],
            'import-x/no-duplicates': 'error',
            'import-x/no-named-as-default': 'error',
            'import-x/no-named-as-default-member': 'error',
            'import-x/no-named-default': 'error',
            'import-x/no-webpack-loader-syntax': 'error',
            'import-x/no-extraneous-dependencies': 'error',
            'import-x/order': [
                'error',
                {
                    groups: [
                        'builtin',
                        'external',
                        'internal',
                        ['parent', 'sibling', 'index'],
                        'type',
                    ],
                    pathGroups: [
                        {
                            pattern: 'react',
                            group: 'external',
                            position: 'before',
                        },
                    ],
                    pathGroupsExcludedImportTypes: ['builtin', 'react'],
                    alphabetize: { order: 'asc', caseInsensitive: true },
                    distinctGroup: false,
                },
            ],
            'unused-imports/no-unused-imports': 'error',
        },
    },
    {
        files: ['theme/src/**/*', 'tests/**/*'],
        rules: {
            'no-restricted-imports': [
                'error',
                {
                    patterns: [
                        {
                            group: ['./', '../'],
                            message: '@で始まる絶対パスを使用してください。',
                        },
                    ],
                },
            ],
        },
    },
    {
        ...eslintPluginPlaywright.configs['flat/recommended'],
        files: ['tests/**/*'],
        rules: {
            ...eslintPluginPlaywright.configs['flat/recommended'].rules,
            'playwright/no-conditional-in-test': 'error',
            'playwright/no-conditional-expect': 'error',
            'playwright/no-skipped-test': 'error',
        },
    },
];
