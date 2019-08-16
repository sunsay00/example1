import * as React from 'react';
import { AsyncStorage } from 'react-native';
import * as UI from '@inf/core-ui';
import { PopUpProvider } from './hooks/usepopup';
import { useBodyScrollLocker } from './hooks/usebodyscrolllocker';

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
    AsyncStorage
  }}>
    {props.children}
  </UI.InjectorProvider>

const ScrollLocker = (props: {
  locked: boolean
  children?: React.ReactNode
}) => {
  useBodyScrollLocker(props.locked);
  return <>{props.children}</>
}

export const Root = (props: {
  children?: (overlays: React.ReactNode) => React.ReactNode
}) =>
  <Injector>
    <UI.LoadingProvider>
      <UI.BreakableProvider>
        <UI.TopViewStackProvider>{overlays =>
          <ScrollLocker locked={!!overlays}>
            <UI.ToastProvider>
              <AlertProvider>
                <PopUpProvider style={{ maxWidth: 500 }}>
                  <UI.AssertSingleton fn={Root}>
                    {props.children && props.children(overlays)}
                  </UI.AssertSingleton>
                </PopUpProvider>
              </AlertProvider>
            </UI.ToastProvider>
          </ScrollLocker>}
        </UI.TopViewStackProvider>
      </UI.BreakableProvider>
    </UI.LoadingProvider>
  </Injector>