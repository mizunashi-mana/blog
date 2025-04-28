import { merge } from 'webpack-merge';
import { BundleAnalyzerPlugin } from 'webpack-bundle-analyzer';
import commonConfig from './webpack.common.config.js';

export default merge(commonConfig, {
    mode: 'development',
    devtool: process.env.DEVTOOL || 'source-map',
    plugins: [
        ...(process.env.ENABLE_ANALYZER === 'true' ? [
            new BundleAnalyzerPlugin(),
        ]: []),
    ],
});
