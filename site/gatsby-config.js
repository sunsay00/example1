// @ts-check

const gatsby = require('gatsby');
const path = require('path');

const env = process.env.GATSBY_ACTIVE_ENV || process.env.NODE_ENV || 'development';
const activeEnv = env == 'development' ? 'dev' : env;

console.log(`Using environment config '${activeEnv}'`);

require('dotenv').config({
  path: `.env.${activeEnv}`
});

/**
 * @type {gatsby.GatsbyConfig & { __experimentalThemes: string[] }}
 */
const config = {
  plugins: [
    'gatsby-plugin-remove-serviceworker',
    'gatsby-plugin-typescript',
    'gatsby-plugin-react-native-web',
    {
      resolve: 'gatsby-source-filesystem',
      options: {
        name: 'images',
        path: path.join(__dirname, 'src', 'images'),
      }
    },
    'gatsby-plugin-sharp',
    'gatsby-transformer-sharp'
  ],
  __experimentalThemes: ['@infng/gatsby-theme-web-ui']
}

module.exports = config;
