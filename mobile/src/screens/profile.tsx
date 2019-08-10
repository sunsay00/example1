import * as React from 'react';
import * as UI from 'core-ui';
import { useAccount } from 'cf-cognito';
import { useNavigation, useNavigationOptions } from '../hooks/usenavigation';

export const Profile = () => {
  const { user, logOut } = useAccount();
  useNavigationOptions({
    title: 'Profile',
    headerRight: (
      <UI.TouchableOpacity onPress={() => logOut()}>
        <UI.View style={{ paddingHorizontal: 16 }}>
          <UI.Text weight="bold">Log out</UI.Text>
        </UI.View>
      </UI.TouchableOpacity>
    ),
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
