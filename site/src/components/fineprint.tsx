import * as React from 'react';
import * as UI from '@inf/core-ui';

export const FinePrint = (props: { secondary?: boolean, children?: React.ReactChild }) =>
  <UI.Text
    color={props.secondary && UI.rgba(UI.Colors.white, .5) || undefined}
    weight="thin"
    style={{
      marginLeft: 16 + 22,
      marginBottom: 16,
      fontSize: 11,
      lineHeight: 14
    }}
  >
    {props.children}
  </UI.Text>

