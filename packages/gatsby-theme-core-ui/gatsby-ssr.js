// @ts-check

const React = require('react');

const gatsby = require('gatsby');

/**
 * @type {gatsby.GatsbySSR['onRenderBody']}
 */
exports.onRenderBody = async ({ setPostBodyComponents }, _pluginOptions) => {
  setPostBodyComponents(
    <script key="ionicons" src="https://unpkg.com/ionicons@4.6.1/dist/ionicons.js"></script>);
}
