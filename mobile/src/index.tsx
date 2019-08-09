import * as React from 'react';
import { StatusBar, } from 'react-native';
import * as UI from 'core-ui';
import * as Mobile from 'mobile-ui';
import {
  createSwitchNavigator, createDrawerNavigator,
  createBottomTabNavigator, createAppContainer, createStackNavigator,
  useNavigation, useNavigationOptions
} from './hooks/usenavigation';
import { Auth } from './components/auth';
import { AccountProvider } from 'cf-cognito';
import { ApolloProvider } from '@inf/apollo';

const Drawer = () =>
  <UI.SafeAreaView>
    <UI.ScrollView>
      <UI.Text>Hello World</UI.Text>
    </UI.ScrollView>
  </UI.SafeAreaView>

const Tab1 = () => {
  const nav = useNavigation();
  useNavigationOptions({
    title: 'wakka',
    headerRight: <UI.View style={{ paddingHorizontal: 16 }}><UI.Text weight="bold">Right</UI.Text></UI.View>,
  });
  return (
    <UI.ScrollView>
      <UI.View style={{ flex: 1, alignItems: 'center', justifyContent: 'center' }}>
        <StatusBar barStyle='dark-content' />
        <UI.SafeAreaView>
          <UI.Accent>123456789</UI.Accent>
          <UI.Header1>Infinage Styleguide</UI.Header1>
          <UI.Header2>Table of Contents</UI.Header2>
          <UI.Header2>Components</UI.Header2>
          <UI.Header4>Accent</UI.Header4>
          <UI.Header2>123456789</UI.Header2>
          <UI.Text weight="black" numberOfLines={1}>123456789</UI.Text>
          <UI.Text weight="bold" numberOfLines={1}>123456789</UI.Text>
          <UI.Text weight="medium" numberOfLines={1}>123456789</UI.Text>
          <UI.Text weight="light" numberOfLines={1}>123456789</UI.Text>
          <UI.Text weight="thin" numberOfLines={1}>123456789</UI.Text>
          <UI.View style={{ flex: 1, justifyContent: 'center', alignItems: 'center' }}>
            <UI.Button onPress={nav.openDrawer}>Open Drawer</UI.Button>
          </UI.View>
        </UI.SafeAreaView>
      </UI.View>
    </UI.ScrollView>
  );
}

const Tab2 = () => {
  const nav = useNavigation();
  useNavigationOptions({
    title: 'wakka2',
    headerLeft: <UI.View style={{ paddingHorizontal: 16 }}><UI.Text weight="bold">Left</UI.Text></UI.View>,
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

const Landing = (props: {}) => {
  const nav = useNavigation();
  return (
    <UI.View style={{ flex: 1, justifyContent: 'center', alignItems: 'center' }}>
      <UI.Header1>Infinage Labs</UI.Header1>
      <UI.Link size="md" onPress={() => nav.navigate('Auth')}>Proceed</UI.Link>
    </UI.View>
  )
}

const Tabs = (initialRouteName: string) => createBottomTabNavigator({
  Tab1: createStackNavigator({ Tab1 }),
  Tab2: createStackNavigator({ Tab2 }),
}, { initialRouteName });

export const Layout = createAppContainer(createSwitchNavigator({
  Main: createDrawerNavigator(
    { Home: Tabs('Tab1'), Settings: Tabs('Tab2') },
    { drawerType: 'slide', contentComponent: Drawer }),
  Landing,
  Auth,
}, { initialRouteName: 'Landing' }));

const MobileRoot = (props: { children?: (overlays: React.ReactNode) => React.ReactNode }) =>
  <Mobile.Injector>
    <UI.LoadingProvider>
      <UI.BreakableProvider>
        <UI.TopViewStackProvider>{overlays =>
          <UI.ToastProvider>
            {props.children && props.children(overlays) || null}
          </UI.ToastProvider>}
        </UI.TopViewStackProvider>
      </UI.BreakableProvider>
    </UI.LoadingProvider>
  </Mobile.Injector>

const config = {
  WEBSOCKET_ENDPOINT: undefined,
  GRAPHQL_ENDPOINT: '',
};

export const App = (props: {}) =>
  <MobileRoot>{overlays =>
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
  </MobileRoot>