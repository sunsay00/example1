import * as React from 'react';
import * as UI from 'core-ui';
import { Slider } from './slider';
import { Icon } from './icon';
import { Alert } from './alert';
import { WebRoot } from './webroot';
import { Image } from './image';
import { ImageBackground } from './imagebackground';
import { Modal } from './modal';

export const Injector = (props: { children?: React.ReactNode }) => {
  return (
    <UI.InjectorProvider client={{
      Slider: props => <Slider {...props} />,
      Alert,
      Icon,
      Image,
      ImageBackground,
      Modal: props => <Modal {...props} />,
    }}>
      <WebRoot>
        {props.children}
      </WebRoot>
    </UI.InjectorProvider>
  );
}