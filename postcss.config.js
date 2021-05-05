module.exports = {
    plugins: [
        /*
        See https://github.com/cssnano/cssnano/issues/1004
        require('cssnano')({
            preset: ['default', {
                mergeRules: false,
            }]
        }),
        */
        require('postcss-preset-env'),
        require('postcss-nested'),
        require('autoprefixer'),
    ]
};
