import * as React from 'react';
import * as UI from 'core-ui';

import { TopViewStackProvider } from '../hooks/usetopviewstack';
import { ToastProvider } from '../hooks/usetoast';
import { AlertProvider } from './alert';
import { ModalProvider } from '../hooks/usemodal';

export const Root = (props: { children?: React.ReactNode }) => {
  return (
    <UI.BreakableProvider>
      <TopViewStackProvider renderWrapper={children =>
        <ToastProvider>
          <AlertProvider>
            <ModalProvider style={{ maxWidth: 500 }}>
              {children}
            </ModalProvider>
          </AlertProvider>
        </ToastProvider>
      }>
        {props.children}
      </TopViewStackProvider>
    </UI.BreakableProvider>
  );
};