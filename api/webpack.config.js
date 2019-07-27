const path = require('path');

module.exports = {
  mode: 'production',
  entry: {
    api: path.resolve('./build/api.js'),
    auth: path.resolve('./build/auth.js'),
  },
  target: 'node',
  output: {
    path: path.resolve('./build'),
    filename: '_[name].js',
    libraryTarget: 'this'
  },
  module: {},
  optimization: { 
    minimize: process.env.NODE_ENV == 'production',
    concatenateModules: true,
  }
};
