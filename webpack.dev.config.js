import { merge } from 'webpack-merge';
import commonConfig from './webpack.common.config.js';
import { BundleAnalyzerPlugin } from 'webpack-bundle-analyzer';

const enableAnalyzer = process.env.ENABLE_ANALYZER === 'true';

export default merge(commonConfig, {
    mode: 'development',
    devtool: process.env.DEVTOOL || 'source-map',
    plugins: [
        ...(enableAnalyzer
            ? [
                    new BundleAnalyzerPlugin(),
                ]
            : []),
    ],
});
