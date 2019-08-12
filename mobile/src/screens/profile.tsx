import * as React from 'react';
import * as UI from 'core-ui';
import { useAccount } from 'cf-cognito';
import { useNavigationOptions } from '../hooks/usenavigation';
import { NavHeaderButton } from '../components/navheaderbutton';

export const Profile = () => {
  const { user, logOut } = useAccount();
  useNavigationOptions({
    title: 'Profile',
    headerRight: <NavHeaderButton onPress={logOut}>Log out</NavHeaderButton>
  });

  if (!user) return <UI.Loading />;

  return (
    <UI.View style={{ flex: 1, justifyContent: 'center', alignItems: 'center' }}>
      <UI.Header3>Username: {user.username}</UI.Header3>
      <UI.Spacer />
      <UI.Header3>Email: {user.email}</UI.Header3>
    </UI.View>
  );
}
