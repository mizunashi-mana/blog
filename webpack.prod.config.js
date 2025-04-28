import { merge } from 'webpack-merge';
import commonConfig from './webpack.common.config.js';

export default merge(commonConfig, {
    mode: 'production',
});
