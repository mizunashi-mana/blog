export default {
    presets: [
        [
            '@babel/preset-env',
            {
                targets: 'defaults',
                useBuiltIns: 'usage',
                corejs: 3,
            },
        ],
    ],
};
