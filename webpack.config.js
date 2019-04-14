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
      vue$: 'vue/dist/vue.esm.js',
      mathjax3$: 'mathjax3/mathjax3/mathjax.js',
      mathjax3: 'mathjax3/mathjax3',
    }
  },

  plugins: [
    // to disable asyncLoad()
    new webpack.NormalModuleReplacementPlugin(
      /AsyncLoad\.js/,
      (resource) => {
        if (resource.context.endsWith('mathjax3/util')) {
          resource.request = resource.request.replace(/AsyncLoad/,"AsyncLoad-disabled");
        }
      }
    )
  ],

  performance: {
    maxEntrypointSize: 1024 * 1024,
    maxAssetSize: 1024 * 1024,
  },
};
