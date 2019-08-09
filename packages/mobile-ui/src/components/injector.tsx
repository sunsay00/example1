import * as React from 'react';
import * as UI from 'core-ui';
import { Slider, Alert, Image, ImageBackground, Modal } from 'react-native';
import { Icon } from './icon';

export const Injector = (props: { children?: React.ReactNode }) => {
  return (
    <UI.InjectorProvider client={{
      Alert,
      Icon,
      Image: props => <Image {...props} />,
      ImageBackground: props => <ImageBackground {...props} />,
      Modal: props => <Modal {...props} />,
      Slider: props => <Slider {...props} />,
    }}>
      {props.children}
    </UI.InjectorProvider>
  );
}