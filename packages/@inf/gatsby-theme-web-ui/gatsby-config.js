// @ts-check

const gatsby = require('gatsby');

/**
 * @type {gatsby.GatsbyConfig}
 */
const config = {
  plugins: [
    {
      resolve: 'gatsby-plugin-page-creator',
      options: {
        path: `${__dirname}/src/pages`
      }
    },
    {
      resolve: 'gatsby-source-filesystem',
      options: {
        name: 'docs',
        path: 'docs'
      }
    },
    'gatsby-plugin-typescript',
    {
      resolve: 'gatsby-plugin-google-fonts',
      options: {
        fonts: [
          'Raleway\:300,400,700,900',
          'Zilla Slab\:300,400,600,700',
        ]
      }
    }
  ]
};

module.exports = config;