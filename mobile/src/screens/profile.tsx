import * as React from 'react';
import * as UI from 'core-ui';
import { useAccount } from 'cf-cognito';
import { useNavigation, useNavigationOptions } from '../hooks/usenavigation';

export const Profile = () => {
  const account = useAccount();
  const nav = useNavigation();
  useNavigationOptions({
    title: 'Profile',
    headerRight: (
      <UI.TouchableOpacity onPress={() => account.logOut()}>
        <UI.View style={{ paddingHorizontal: 16 }}>
          <UI.Text weight="bold">Log out</UI.Text>
        </UI.View>
      </UI.TouchableOpacity>
    ),
  });
  return (
    <UI.View style={{ flex: 1, justifyContent: 'center', alignItems: 'center' }}>
      <UI.Header1>Tab2</UI.Header1>
      <UI.Button onPress={nav.openDrawer}>
        Open Drawer
    </UI.Button>
    </UI.View>
  );
}
