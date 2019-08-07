import * as React from 'react';
import * as UI from 'core-ui';
import { TopViewStackProvider } from '../hooks/usetopviewstack';
import { ToastProvider } from '../hooks/usetoast';
import { AlertProvider } from './alert';
import { ModalProvider } from '../hooks/usemodal';

export const WebRoot = (props: { children?: React.ReactNode }) =>
  <UI.BreakableProvider>
    <TopViewStackProvider renderWrapper={elem =>
      <ToastProvider>
        <AlertProvider>
          <ModalProvider style={{ maxWidth: 500 }}>
            {elem}
          </ModalProvider>
        </AlertProvider>
      </ToastProvider>
    }>
      {props.children}
    </TopViewStackProvider>
  </UI.BreakableProvider>
