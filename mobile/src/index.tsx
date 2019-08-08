import * as React from 'react';
import { StatusBar, } from 'react-native';
import * as UI from 'core-ui';
import {
  createSwitchNavigator, createDrawerNavigator,
  createBottomTabNavigator, createAppContainer, createStackNavigator,
  useNavigation, useNavigationOptions
} from './hooks/usenavigation';

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

const Tabs = (initialRouteName: string) => createBottomTabNavigator({
  Tab1: createStackNavigator({ Tab1 }),
  Tab2: createStackNavigator({ Tab2 }),
}, { initialRouteName });

export const App = createAppContainer(createSwitchNavigator({
  Main: createDrawerNavigator(
    { Home: Tabs('Tab1'), Settings: Tabs('Tab2') },
    { drawerType: 'slide', contentComponent: Drawer }),
  Landing: () => {
    const nav = useNavigation();
    return (
      <UI.View style={{ flex: 1, justifyContent: 'center', alignItems: 'center' }}>
        <UI.Header1>Landing</UI.Header1>
        <UI.Button onPress={() => nav.navigate('Auth')}>Log in</UI.Button>
      </UI.View>
    );
  },
  Auth: createStackNavigator({
    LogIn: () => {
      const nav = useNavigation();
      return (
        <UI.View style={{ flex: 1, justifyContent: 'center', alignItems: 'center' }}>
          <UI.Header1>Log in</UI.Header1>
          <UI.Button onPress={() => nav.navigate('Main')}>Sign in</UI.Button>
          <UI.Spacer />
          <UI.Button onPress={() => nav.push('SignUp')}>Sign up</UI.Button>
          <UI.Spacer />
          <UI.Button size="xs" secondary onPress={() => nav.navigate('Landing')}>Landing</UI.Button>
        </UI.View>
      );
    },
    SignUp: () => {
      const nav = useNavigation();
      return (
        <UI.View style={{ flex: 1, justifyContent: 'center', alignItems: 'center' }}>
          <UI.Header1>Sign up</UI.Header1>
          <UI.Button onPress={() => nav.navigate('LogIn')}>Go To Log in</UI.Button>
        </UI.View>
      );
    }
  }, { headerMode: 'none' }),
}, { initialRouteName: 'Landing' }));