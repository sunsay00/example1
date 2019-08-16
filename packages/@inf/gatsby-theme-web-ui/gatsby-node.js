// @ts-check

const gatsby = require('gatsby');

/**
 * @type {gatsby.GatsbyNode['createPages']}
 */
exports.createPages = async ({ graphql, actions }) => {
  /*
  const result = await graphql(`{
    allFile(filter: { sourceInstanceName: { eq: "docs" } }) {
      edges {
        node {
          name
          childMdx {
            code {
              body
            }
          }
        }
      }
    }
  }`);

  const pages = result.data.allFile.edges.map(({ node }) => node);

  pages.forEach(page => {
    actions.createPage({
      path: `/${page.name}`,
      component: require.resolve('./src/templates/docs.tsx'),
      context: {
        body: page.childMdx.code.body
      }
    });
  });
  */
}

/*
exports.onCreateWebpackConfig = ({
  stage,
  rules,
  loaders,
  plugins,
  actions,
}) => {
  actions.setWebpackConfig({
    module: {
      rules: [
        {
          test: /\.mdxx/,
          use: [
            'babel-loader',
            '@mdx-js/loader'
          ],
        },
      ],
    }
  })
}
*/