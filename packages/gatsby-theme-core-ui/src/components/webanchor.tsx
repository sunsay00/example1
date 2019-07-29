import * as React from 'react';
import { Colors } from 'core-ui';

export const WebAnchor = (props: { style: {} }) =>
  <a {...props} style={{ ...props.style, textDecoration: 'none', color: Colors.accentBlue }} />
