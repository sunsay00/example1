import * as React from 'react';
import * as UI from 'core-ui';
import { Slider, Alert, Image, ImageBackground, Modal } from 'react-native';
import { Icon, Icon2 } from './components/inject/icon';
import { FixedView } from './components/inject/fixedview';

export const Injector = (props: { children?: React.ReactNode }) =>
  <UI.InjectorProvider client={{
    Alert,
    Icon,
    Icon2,
    Image: props => <Image {...props} />,
    ImageBackground: props => <ImageBackground {...props} />,
    Modal: props => <Modal {...props} />,
    Slider: props => <Slider {...props} />,
    FixedView,
  }}>
    {props.children}
  </UI.InjectorProvider>

export const Root = (props: { children?: (overlays: React.ReactNode) => React.ReactNode }) =>
  <Injector>
    <UI.LoadingProvider>
      <UI.BreakableProvider>
        <UI.TopViewStackProvider>{overlays =>
          <UI.ToastProvider>
            {props.children && props.children(overlays) || null}
          </UI.ToastProvider>}
        </UI.TopViewStackProvider>
      </UI.BreakableProvider>
    </UI.LoadingProvider>
  </Injector>