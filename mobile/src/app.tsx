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
import { StyleGuide } from './screens/styleguide';

const BottomTabButton = (props: { iconName: UI.IconName, routeName: string, focused: boolean }) => {
  const nav = useNavigation();
  return (
    <UI.Icon name={props.iconName} size="sm" onPress={() => nav.navigate(props.routeName)} disabled={!props.focused} />
  );
}

const Tabs = (initialRouteName: string) => createBottomTabNavigator({
  Home: {
    screen: createStackNavigator({ Home }),
    navigationOptions: { tabBarIcon: (props: any) => <BottomTabButton iconName="home" routeName="Home" focused={props.focused} /> },
  },
  StyleGuide: {
    screen: createStackNavigator({ StyleGuide }),
    navigationOptions: { tabBarIcon: (props: any) => <BottomTabButton iconName="brush" routeName="StyleGuide" focused={props.focused} /> },
  },
  Profile: {
    screen: createStackNavigator({ Profile }),
    navigationOptions: { tabBarIcon: (props: any) => <BottomTabButton iconName="person" routeName="Profile" focused={props.focused} /> },
  }
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