// @ts-check
import * as React from 'react';
import * as Web from '@inf/gatsby-theme-web-ui';
import { App } from './src/app';

export const wrapPageElement = ({ element }) => (
  <Web.SSRProvider enabled={true}>
    <App>{element}</App>
  </Web.SSRProvider>
);
