import * as React from 'react';
import * as UI from 'core-ui';
import * as Mobile from 'mobile-ui';
import { AccountProvider } from 'cf-cognito';
import {
  createSwitchNavigator, createDrawerNavigator, createBottomTabNavigator, createAppContainer,
  createStackNavigator, useNavigation, createNavWrapper,
} from './hooks/usenavigation';
import { AuthGuard } from './components/authguard';
import { Screens } from './components/auth';
import { Landing } from './screens/landing';
import { ApolloProvider } from '@inf/apollo';
import { Profile } from './screens/profile';
import { Drawer } from './screens/drawer';
import { Home } from './screens/home';
import { StyleGuide } from './screens/styleguide';
import { ExampleKeyboardAccessoryView } from './components/examplekeyboardaccessoryview';

const config = {
  WEBSOCKET_ENDPOINT: undefined,
  GRAPHQL_ENDPOINT: '',
};

const BottomTabButton = (props: { iconName: UI.IconName, routeName: string, focused: boolean }) => {
  const nav = useNavigation();
  return (
    <UI.Icon name={props.iconName} size="sm" onPress={() => nav.navigate(props.routeName)} disabled={!props.focused} />
  );
}

const Tabs = (initialRouteName: string) => createBottomTabNavigator({
  Home: {
    screen: createStackNavigator({ Home: () => <ExampleKeyboardAccessoryView><Home /></ExampleKeyboardAccessoryView> }),
    navigationOptions: {
      tabBarIcon: (props: any) => <BottomTabButton iconName="home" routeName="Home" focused={props.focused} />,
    }
  },
  StyleGuide: {
    screen: createStackNavigator({ StyleGuide: () => <ExampleKeyboardAccessoryView><StyleGuide /></ExampleKeyboardAccessoryView> }),
    navigationOptions: {
      tabBarIcon: (props: any) => <BottomTabButton iconName="brush" routeName="StyleGuide" focused={props.focused} />,
    }
  },
  Profile: {
    screen: createStackNavigator({ Profile: () => <ExampleKeyboardAccessoryView><Profile /></ExampleKeyboardAccessoryView> }),
    navigationOptions: {
      tabBarIcon: (props: any) => <BottomTabButton iconName="person" routeName="Profile" focused={props.focused} />,
    }
  }
}, { initialRouteName });

const Main = createDrawerNavigator(
  { Home: Tabs('Home'), StyleGuide: Tabs('StyleGuide'), Profile: Tabs('Profile') },
  { drawerType: 'slide', contentComponent: Drawer });

const Layout = createAppContainer(createNavWrapper(
  props => <AuthGuard>{props.children}</AuthGuard>,
  createSwitchNavigator({
    Loading: UI.Loading,
    Guest: createStackNavigator({
      Landing,
      LogIn: Screens.LogInScreen,
      SignUp: Screens.SignUpScreen,
      Confirmation: Screens.ConfirmationScreen,
      ForgotPassword: Screens.ForgotPasswordScreen,
      ResetPassword: Screens.ResetPasswordScreen,
    }, { initialRouteName: 'Landing', defaultNavigationOptions: { headerTransparent: true } }),
    Main,
  }, { initialRouteName: 'Loading' })
));

export const App = (props: {}) =>
  <Mobile.Root>{overlays =>
    <AccountProvider region="us-east-1">
      <ApolloProvider
        authorization="Guest"
        websocketEndpoint={config.WEBSOCKET_ENDPOINT}
        graphqlEndpoint={config.GRAPHQL_ENDPOINT}
      >
        <UI.AssertSingleton fn={App}>
          <Layout />
          {overlays}
        </UI.AssertSingleton>
      </ApolloProvider>
    </AccountProvider>}
  </Mobile.Root>