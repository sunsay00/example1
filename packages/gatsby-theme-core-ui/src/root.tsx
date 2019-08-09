import * as React from 'react';
import * as UI from 'core-ui';
import { PopUpProvider } from './hooks/usepopup';

import { Alert, AlertProvider } from './components/inject/alert';
import { Slider } from './components/inject/slider';
import { Icon } from './components/inject/icon';
import { Image } from './components/inject/image';
import { ImageBackground } from './components/inject/imagebackground';
import { Modal } from './components/inject/modal';
import { FixedView } from './components/inject/fixedview';

const Injector = (props: { children?: React.ReactNode }) =>
  <UI.InjectorProvider client={{
    Alert,
    FixedView,
    Icon,
    Image,
    ImageBackground,
    Modal: props => <Modal {...props} />,
    Slider: props => <Slider {...props} />,
  }}>
    {props.children}
  </UI.InjectorProvider>

export const Root = (props: {
  children?: (overlays: React.ReactNode) => React.ReactNode
}) =>
  <Injector>
    <UI.LoadingProvider>
      <UI.BreakableProvider>
        <UI.TopViewStackProvider>{overlays =>
          <UI.ToastProvider>
            <AlertProvider>
              <PopUpProvider style={{ maxWidth: 500 }}>
                {props.children && props.children(overlays)}
              </PopUpProvider>
            </AlertProvider>
          </UI.ToastProvider>}
        </UI.TopViewStackProvider>
      </UI.BreakableProvider>
    </UI.LoadingProvider>
  </Injector>