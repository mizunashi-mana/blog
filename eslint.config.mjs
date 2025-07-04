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
        files: ['theme/src/**/*', 'tests/**/*'],
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
