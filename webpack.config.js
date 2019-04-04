const webpack = require('webpack');
const path = require('path');

module.exports = {
  mode: 'production',

  entry: {
    bundle: './js/index.js',
  },

	output: {
		filename: '[name].js',
    path: path.resolve(__dirname, 'content/dist-asset')
  },

  resolve: {
    alias: {
      handlebars: 'handlebars/dist/handlebars.min.js',
      vue$: 'vue/dist/vue.esm.js',
    }
  },

  performance: {
    maxEntrypointSize: 400 * 1024,
    maxAssetSize: 400 * 1024,
  },
};
