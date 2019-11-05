// @ts-check
import * as React from 'react';
import * as UI from '@infng/core-ui';
import { App } from './src/app';

export const wrapPageElement = ({ element }) => (
  <UI.SSRProvider enabled={true}>
    <App>{element}</App>
  </UI.SSRProvider>
);
