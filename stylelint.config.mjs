/** @type {import('stylelint').Config} */
export default {
    extends: [
        'stylelint-config-standard-scss',
        'stylelint-prettier/recommended',
    ],
    rules: {
        'no-descending-specificity': null,
    },
};
