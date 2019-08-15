import * as React from 'react';
import * as UI from 'core-ui';
import { useAccount } from 'cf-cognito';
import { useNavOptions, useNav, NavHeaderButton } from '../hooks/usenav';

export const Profile = () => {
  const nav = useNav();
  const { user, logOut } = useAccount();
  useNavOptions({
    title: 'Profile',
    headerLeft: <NavHeaderButton prefixIconName="menu" onPress={nav.openDrawer} />,
    headerRight: <NavHeaderButton onPress={logOut}>Log out</NavHeaderButton>
  });

  if (!user) return <UI.Loading />;

  return (
    <UI.View style={{ flex: 1, justifyContent: 'center', alignItems: 'center', backgroundColor: UI.rgba(UI.Colors.black, .05) }}>
      <UI.Header3>Username: {user.username}</UI.Header3>
      <UI.Spacer />
      <UI.Header3>Email: {user.email}</UI.Header3>
    </UI.View>
  );
}
