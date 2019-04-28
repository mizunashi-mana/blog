const path = require('path');
const MiniCssExtractPlugin = require('mini-css-extract-plugin');

module.exports = {
  mode: 'production',

  entry: {
    bundle: './frontend/index.js',
  },

  output: {
    filename: '[name].js',
    path: path.resolve(__dirname, 'content/dist-asset')
  },

  resolve: {
    alias: {
      vue$: 'vue/dist/vue.esm.js',
      katex$: 'katex/dist/katex.mjs',
      katex: 'katex',
    }
  },

  module: {
    rules: [
      {
        test: /\.(scss|css)$/,
        use: [
          MiniCssExtractPlugin.loader,
          {
            loader: 'css-loader',
          },
          {
            loader: 'postcss-loader',
            options: {
              plugins: function () {
                return [
                  require('cssnano')({
                    preset: ['default', {
                      mergeRules: false,
                    }]
                  }),
                  require('precss'),
                  require('autoprefixer')
                ];
              }
            }
          },
        ]
      },
      {
        test: /\.(ttf|woff2?)$/,
        use: [
          {
            loader: 'file-loader'
          },
        ]
      }
    ],
  },

  plugins: [
    new MiniCssExtractPlugin({
      filename: '[name].css'
    })
  ],

  performance: {
    maxEntrypointSize: 1024 * 1024,
    maxAssetSize: 1024 * 1024,
  },
};
