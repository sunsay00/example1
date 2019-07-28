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
    module: {
      rules: [
        { test: /\.flow$/, use: 'ignore-loader' }
      ],
    }
  })
}