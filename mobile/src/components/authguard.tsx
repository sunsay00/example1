import * as React from 'react';
import { useAccount } from 'cf-cognito';
import { useNavigation } from '../hooks/usenavigation';

export const AuthGuard = (props: { children?: React.ReactNode }) => {
  const account = useAccount();
  const nav = useNavigation();
  React.useEffect(() => {
    if (account.ready) {
      if (account.user) nav.navigate('Main');
      else nav.navigate('Guest');
    }
  }, [account.ready, account.user]);
  return <>{props.children}</>;
}

