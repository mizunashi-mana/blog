import path from 'path';
import MiniCssExtractPlugin from 'mini-css-extract-plugin';

export default {
    mode: 'none',
    target: 'web',
    entry: {
        app: './theme/src/index.ts',
    },
    output: {
        filename: 'bundle.[name].js',
        chunkFilename: 'bundle.[name].[contenthash].js',
        path: path.resolve(import.meta.dirname, 'theme/content/static/dist-assets'),
        clean: true,
    },

    resolve: {
        alias: {
            katex$: 'katex/dist/katex.mjs',
            katex: 'katex',
        }
    },

    module: {
        rules: [
            {
                test: /\.tsx?$/,
                use: 'ts-loader',
            },
            {
                test: /\.(scss|css)$/,
                use: [
                    MiniCssExtractPlugin.loader,
                    {
                        loader: 'css-loader',
                    },
                    {
                        loader: 'postcss-loader',
                    },
                    {
                        loader: 'sass-loader',
                    },
                ],
            },
            {
                test: /\.(woff|woff2|eot|ttf|otf|svg)$/,
                type: 'asset/resource',
            },
        ],
    },

    resolve: {
        extensions: ['.tsx', '.ts', '.js', '.css', '.scss'],
    },

    plugins: [
        new MiniCssExtractPlugin({
            filename: 'bundle.[name].css',
            chunkFilename: 'bundle.[name].[contenthash].css',
        }),
    ],

    performance: {
        maxAssetSize: 512 * 1024,
    },
};
