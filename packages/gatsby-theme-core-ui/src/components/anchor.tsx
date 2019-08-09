import * as React from 'react';
import { Colors } from 'core-ui';

export const Anchor = (props: { style: {} }) =>
  <a {...props} style={{ ...props.style, textDecoration: 'none', color: Colors.accentBlue }} />
