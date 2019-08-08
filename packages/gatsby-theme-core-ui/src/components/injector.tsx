import * as React from 'react';
import * as UI from 'core-ui';
import { Slider } from './slider';
import { Icon } from './icon';

export const Injector = (props: { children?: React.ReactNode }) => {
  return (
    <UI.InjectorProvider client={{
      Slider: props => <Slider {...props} />,
      Icon: props => <Icon {...props} />,
    }}>
      {props.children}
    </UI.InjectorProvider>
  );
}