import * as React from 'react';
import { sg } from 'core-ui';

export const WebAnchor = (props: any) =>
  <a {...props} style={{ ...props.style, textDecoration: 'none', color: sg.colors.accentBlue }} />
