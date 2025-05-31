import globals from 'globals';
import typescriptEslint from 'typescript-eslint';
import eslintJs from '@eslint/js';
import stylistic from '@stylistic/eslint-plugin';

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
        files: ['theme/src/**/*'],
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
];
