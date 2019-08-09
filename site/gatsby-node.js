// @ts-check

const gatsby = require('gatsby');

/**
 * @type {gatsby.GatsbyNode['onCreateWebpackConfig']}
 */
exports.onCreateWebpackConfig = ({
  stage,
  rules,
  loaders,
  plugins,
  actions,
}) => {
  actions.setWebpackConfig({
    devServer: {
      hot: false
    },
    //devtool: 'cheap-module-eval-source-map', // fast inpercise debugging
    devtool: 'source-map', // slow percise debugging
    module: {
      rules: [
        { test: /\.flow$/, use: 'ignore-loader' },
        { test: /\.js$/, enforce: 'pre', loader: "source-map-loader" }
      ]
    },
  })
}
