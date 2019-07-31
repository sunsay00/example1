import * as React from 'react';
import { AsyncStorage } from 'react-native';
import { Account, UserPoolMode } from 'cf-cognito';

const _account = new Account(UserPoolMode.Web, AsyncStorage);

export const AccountContext = React.createContext<Account>(_account);

export const AccountProvider = (props: { region: string, children?: React.ReactNode }) => {
  const [ready, setReady] = React.useState(false);

  React.useEffect(() => {
    _account.init(props.region)
      .then(ready => setReady(ready))
      .catch(console.error);
  }, []);

  if (!ready) return null;
  return <AccountContext.Provider value={_account}>{props.children}</AccountContext.Provider>;
}