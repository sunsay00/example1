import * as React from 'react';
import * as UI from 'core-ui';
import { useAccount } from 'cf-cognito';
import * as Mobile from 'mobile-ui';
import {
  createSwitchNavigator, createDrawerNavigator, createBottomTabNavigator, createAppContainer,
  createStackNavigator, useNavigation, createNavWrapper,
} from './hooks/usenavigation';
import { Auth } from './components/auth';
import { Landing } from './screens/landing';
import { AccountProvider } from 'cf-cognito';
import { ApolloProvider } from '@inf/apollo';
import { Profile } from './screens/profile';
import { Drawer } from './screens/drawer';
import { Home } from './screens/home';

const Tabs = (initialRouteName: string) => createBottomTabNavigator({
  Home: createStackNavigator({ Home }),
  Profile: createStackNavigator({ Profile }),
}, { initialRouteName });

const AuthGuard = (props: { children?: React.ReactNode }) => {
  const account = useAccount();
  const nav = useNavigation();
  React.useEffect(() => {
    if (account.ready) {
      if (account.user) nav.navigate('Main');
      else nav.navigate('Landing');
    }
  }, [account.ready, account.user]);
  return <>{props.children}</>;
}

const Main = createDrawerNavigator(
  { Home: Tabs('Home'), Profile: Tabs('Profile') },
  { drawerType: 'slide', contentComponent: Drawer });

export const Layout = createAppContainer(createNavWrapper(
  children => <AuthGuard>{children}</AuthGuard>,
  createSwitchNavigator({
    Loading: UI.Loading,
    Landing,
    Auth,
    Main,
  }, { initialRouteName: 'Loading' })
));

const config = {
  WEBSOCKET_ENDPOINT: undefined,
  GRAPHQL_ENDPOINT: '',
};

export const App = (props: {}) =>
  <Mobile.Root>{overlays =>
    <AccountProvider region="us-east-1">
      <ApolloProvider
        authorization="Guest"
        websocketEndpoint={config.WEBSOCKET_ENDPOINT}
        graphqlEndpoint={config.GRAPHQL_ENDPOINT}
      >
        <Layout />
        {overlays}
      </ApolloProvider>
    </AccountProvider>}
  </Mobile.Root>