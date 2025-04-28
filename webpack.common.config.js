import path from 'path';
import MiniCssExtractPlugin from 'mini-css-extract-plugin';
import SvgChunkWebpackPlugin from 'svg-chunk-webpack-plugin';

export default {
    mode: 'none',
    target: 'web',
    entry: {
        app: './frontend/index.ts',
    },
    output: {
        filename: 'bundle.[name].js',
        path: path.resolve(import.meta.dirname, 'content/dist-asset'),
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
                test: /\.(ttf|woff2?)$/,
                type: 'asset/resource',
            },
            {
                test: /\.svg$/,
                use: [
                    {
                        loader: SvgChunkWebpackPlugin.loader
                    },
                ],
            },
        ],
    },

    resolve: {
        extensions: ['.tsx', '.ts', '.js'],
    },

    plugins: [
        new MiniCssExtractPlugin({
            filename: 'bundle.[name].css'
        }),
        new SvgChunkWebpackPlugin(),
    ],

    performance: {
        maxEntrypointSize: 512 * 1024,
        maxAssetSize: 1024 * 1024,
    },
};
