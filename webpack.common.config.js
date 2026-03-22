import path from 'node:path';
import MiniCssExtractPlugin from 'mini-css-extract-plugin';
import TsconfigPathsPlugin from 'tsconfig-paths-webpack-plugin';

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
        extensions: ['.tsx', '.ts', '.js', '.css', '.scss'],
        plugins: [new TsconfigPathsPlugin()],
    },

    module: {
        rules: [
            {
                test: /\.tsx?$/v,
                use: [
                    {
                        loader: 'babel-loader',
                        options: {},
                    },
                    {
                        loader: 'ts-loader',
                    },
                ],
            },
            {
                test: /\.(?:scss|css)$/v,
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
                test: /\.(?:woff|woff2|eot|ttf|otf|svg)$/v,
                type: 'asset/resource',
            },
        ],
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
