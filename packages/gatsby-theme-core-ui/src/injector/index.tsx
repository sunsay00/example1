import * as React from 'react';
import * as UI from 'core-ui';

import { Slider } from './components/slider';
import { Icon } from './components/icon';
import { Alert } from './components/alert';
import { Image } from './components/image';
import { ImageBackground } from './components/imagebackground';
import { Modal } from './components/modal';
import { useToast } from './hooks/usetoast';

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