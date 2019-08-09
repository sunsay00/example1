import * as React from 'react';
import * as UI from 'core-ui';

import { TopViewStackProvider } from '../hooks/usetopviewstack';
import { ToastProvider } from '../hooks/usetoast';
import { AlertProvider } from './alert';
import { PopUpProvider } from '../hooks/usepopup';

export const Providers = (props: { children?: React.ReactNode }) => {
  return (
    <UI.BreakableProvider>
      <TopViewStackProvider renderWrapper={children =>
        <ToastProvider>
          <AlertProvider>
            <PopUpProvider style={{ maxWidth: 500 }}>
              {children}
            </PopUpProvider>
          </AlertProvider>
        </ToastProvider>
      }>
        {props.children}
      </TopViewStackProvider>
    </UI.BreakableProvider>
  );
};