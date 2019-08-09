import * as React from 'react';
import * as UI from 'core-ui';

import { Slider } from './slider';
import { Icon } from './icon';
import { Alert } from './alert';
import { Image } from './image';
import { ImageBackground } from './imagebackground';
import { Modal } from './modal';
import { useToast } from '../hooks/usetoast';

export const Injector = (props: { children?: React.ReactNode }) => {
  const toast = useToast();
  return (
    <UI.InjectorProvider client={{
      Slider: props => <Slider {...props} />,
      Alert,
      Icon,
      Image,
      ImageBackground,
      toast,
      Modal: props => <Modal {...props} />,
    }}>
      {props.children}
    </UI.InjectorProvider>
  );
};