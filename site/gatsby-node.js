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
    //devtool: 'source-map', // uncomment this line percise debugging is required (really slow)
    module: {
      rules: [
        { test: /\.flow$/, use: 'ignore-loader' },
        { test: /\.js$/, enforce: 'pre', loader: "source-map-loader" }
      ]
    },
  })
}
