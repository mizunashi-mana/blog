module.exports = {
    plugins: [
        require('cssnano')({
            preset: ['default', {
                mergeRules: false,
            }]
        }),
        require('postcss-preset-env'),
        require('autoprefixer'),
    ]
};
