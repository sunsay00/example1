import * as React from 'react';
import { useAccount } from '@inf/cf-cognito';
import { useNav } from '../hooks/usenav';

export const AuthGuard = (props: { children?: React.ReactNode }) => {
  const account = useAccount();
  const nav = useNav();
  React.useEffect(() => {
    if (account.ready) {
      if (account.user) nav.navigate('Main');
      else nav.navigate('Guest');
    }
  }, [account.ready, account.user]);
  return <>{props.children}</>;
}

