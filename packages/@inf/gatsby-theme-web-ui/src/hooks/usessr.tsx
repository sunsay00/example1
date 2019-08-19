import * as React from 'react';

type ContextValue = boolean;

const SSRContext = React.createContext<ContextValue | undefined>(undefined);

export const useSSR = () => {
  const ret = React.useContext(SSRContext);
  if (ret == undefined) throw new Error('invalid ssr context');
  return ret;
}

export const SSRProvider = (props: { enabled: boolean, children?: React.ReactNode }) =>
  <SSRContext.Provider value={props.enabled}>
    {props.children}
  </SSRContext.Provider>
