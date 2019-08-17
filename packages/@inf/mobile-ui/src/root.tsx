import * as React from 'react';
import * as UI from '@inf/core-ui';
import { Keyboard, Slider, Alert, Image, ImageBackground, Modal } from 'react-native';
import { Icon } from './components/inject/icon';
import { FixedView } from './components/inject/fixedview';
import { KeyboardProvider } from './hooks/usekeyboard';
import AsyncStorage from '@react-native-community/async-storage';

export const Injector = (props: { children?: React.ReactNode }) =>
  <UI.InjectorProvider client={{
    Alert,
    Icon,
    Image: props => <Image {...props} />,
    ImageBackground: props => <ImageBackground {...props} />,
    Modal: props => <Modal {...props} />,
    Slider: props => <Slider {...props} />,
    FixedView,
    AsyncStorage
  }}>
    {props.children}
  </UI.InjectorProvider>

export const Root = (props: { children?: (overlays: React.ReactNode) => React.ReactNode }) =>
  <Injector>
    <UI.LoadingProvider>
      <UI.BreakableProvider>
        <UI.TopViewStackProvider>{overlays =>
          <UI.ToastProvider>
            <UI.View style={{ flex: 1 }} onStartShouldSetResponder={() => { Keyboard.dismiss(); return false; }}>
              <KeyboardProvider>
                <UI.AssertSingleton fn={Root}>
                  {props.children && props.children(overlays) || null}
                </UI.AssertSingleton>
              </KeyboardProvider>
            </UI.View>
          </UI.ToastProvider>}
        </UI.TopViewStackProvider>
      </UI.BreakableProvider>
    </UI.LoadingProvider>
  </Injector>