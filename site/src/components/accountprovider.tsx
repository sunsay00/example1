import * as React from 'react';
import { AsyncStorage } from 'react-native';
import * as UI from 'gatsby-theme-core-ui';
import { Account, UserPoolMode } from 'cf-cognito';

const _account = new Account(UserPoolMode.Web, AsyncStorage);

export const AccountContext = React.createContext<Account>(_account);

export const AccountProvider = (props: { region: string, children?: React.ReactNode }) => {
  const [ready, setReady] = React.useState(false);

  React.useEffect(() => {
    _account.init(props.region)
      .then(() => setReady(true))
      .catch(console.error);
  }, []);

  console.log('ready', ready);

  if (!ready) return <UI.Loading />;

  return <AccountContext.Provider value={_account}>{props.children}</AccountContext.Provider>;
}