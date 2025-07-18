import globals from 'globals';
import typescriptEslint from 'typescript-eslint';
import eslintJs from '@eslint/js';
import stylistic from '@stylistic/eslint-plugin';
import eslintPluginPlaywright from 'eslint-plugin-playwright';

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
    ...typescriptEslint.config(
        typescriptEslint.configs.recommended,
        typescriptEslint.configs.strict,
        typescriptEslint.configs.recommendedTypeChecked,
        typescriptEslint.configs.stylisticTypeChecked,
        {
            files: ['**/*.{js,mjs}'],
            extends: [typescriptEslint.configs.disableTypeChecked],
        },
    ),
];
