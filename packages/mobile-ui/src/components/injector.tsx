import * as React from 'react';
import * as UI from 'core-ui';
import { Slider, Alert, Image, ImageBackground } from 'react-native';
import { Icon } from './icon';

export const Injector = (props: { children?: React.ReactNode }) => {
  return (
    <UI.InjectorProvider client={{
      Slider: props => <Slider {...props} />,
      Icon,
      Image: props => <Image {...props} />,
      ImageBackground: props => <ImageBackground {...props} />,
      Alert,
    }}>
      {props.children}
    </UI.InjectorProvider>
  );
}