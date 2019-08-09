import * as React from 'react';
import * as UI from 'core-ui';
import { Slider, Alert, Image, ImageBackground, Modal } from 'react-native';
import { Icon } from './components/icon';

export const Injector = (props: { children?: React.ReactNode }) => {
  return (
    <UI.InjectorProvider client={{
      Alert,
      Icon,
      Image: props => <Image {...props} />,
      ImageBackground: props => <ImageBackground {...props} />,
      Modal: props => <Modal {...props} />,
      Slider: props => <Slider {...props} />,
      toast: {
        info: (msg: string, onEnd?: () => void) => console.info(msg),
        warn: (msg: string, onEnd?: () => void) => console.warn(msg),
        error: (msg: string, onEnd?: () => void) => console.error(msg),
        success: (msg: string, onEnd?: () => void) => console.log(msg),
      }
    }}>
      {props.children}
    </UI.InjectorProvider>
  );
}