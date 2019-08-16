import * as React from 'react';
import { Colors } from '@inf/core-ui';

export const Anchor = (props: { style: {} }) =>
  <a {...props} style={{ ...props.style, textDecoration: 'none', color: Colors.accentBlue }} />
