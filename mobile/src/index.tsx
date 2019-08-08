import * as React from 'react';
import { SafeAreaView, StatusBar, } from 'react-native';
import * as UI from 'core-ui';
import {
  NavigationTransitionProps, createSwitchNavigator, createDrawerNavigator,
  createBottomTabNavigator, createAppContainer, createStackNavigator
} from "react-navigation";
import { useNavigation } from './hooks/usenavigation';

const Tab1 = (props: {}) => {
  const nav = useNavigation();
  return (
    <UI.View style={{ flex: 1, alignItems: 'center', justifyContent: 'center' }}>
      <StatusBar barStyle='dark-content' />
      <SafeAreaView>
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
          <UI.Button onPress={nav.openDrawer}>
            Open Drawer
        </UI.Button>
          <UI.Text>Home</UI.Text>
        </UI.View>
      </SafeAreaView>
    </UI.View>
  );
}

const Tab2 = (props: {}) => {
  const nav = useNavigation();
  return (
    <UI.View style={{ flex: 1, justifyContent: 'center', alignItems: 'center' }}>
      <UI.Button onPress={nav.openDrawer}>
        Open Drawer
    </UI.Button>
      <UI.Text style={{ fontWeight: 'bold', marginTop: 20 }}>Settings</UI.Text>
    </UI.View>
  );
}

const transitionConfig = () => ({
  transitionSpec: {
    duration: 400,
    easing: UI.Easing.out(UI.Easing.poly(4)),
    timing: UI.Animated.timing,
    useNativeDriver: true,
  },
  screenInterpolator: (sceneProps: NavigationTransitionProps) => {
    const { layout, position, scene } = sceneProps
    const thisSceneIndex = scene.index
    const width = layout.initWidth
    const translateX = position.interpolate({
      inputRange: [thisSceneIndex - 1, thisSceneIndex],
      outputRange: [width, 0],
    });
    return { transform: [{ translateX }] }
  },
});

export const App = createAppContainer(createSwitchNavigator({
  Main: createDrawerNavigator(
    {
      Home: createBottomTabNavigator({ Tab1, Tab2 }, { initialRouteName: 'Tab1' }),
      Settings: createBottomTabNavigator({ Tab1, Tab2 }, { initialRouteName: 'Tab2' }),
    }, {
      //hideStatusBar: true,
      //drawerBackgroundColor: 'rgba(255,255,255,.9)',
      //overlayColor: '#6b52ae',
      //contentOptions: { activeTintColor: '#fff', activeBackgroundColor: '#6b52ae' },
    }
  ),
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
  }, { transitionConfig }),
}, { initialRouteName: 'Landing' }));