import * as React from 'react';
import * as UI from 'core-ui';
import { Slider } from './slider';
import { Icon } from './icon';
import { Alert } from './alert';
import { WebRoot } from './webroot';
import { Image } from './image';
import { ImageBackground } from './imagebackground';

export const Injector = (props: { children?: React.ReactNode }) => {
  return (
    <UI.InjectorProvider client={{
      Slider: props => <Slider {...props} />,
      Icon,
      Image,
      ImageBackground,
      Alert,
    }}>
      <WebRoot>
        {props.children}
      </WebRoot>
    </UI.InjectorProvider>
  );
}