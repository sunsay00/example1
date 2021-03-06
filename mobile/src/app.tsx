import * as React from 'react';
import * as UI from '@infng/core-ui';
import * as Mobile from '@infng/mobile-ui';
import { AccountProvider, AccountConsumer } from '@infng/cf-cognito';
import {
  createSwitchNavigator, createDrawerNavigator, createBottomTabNavigator, createAppContainer,
  createStackNavigator, createTabBarIcon, createNavWrapper,
} from './hooks/usenav';
import { AuthGuard } from './components/authguard';
import { Screens } from './components/auth';
import { Landing } from './screens/landing';
import { ApolloProvider } from '@infng/apollo';
import { Profile } from './screens/profile';
import { Drawer } from './screens/drawer';
import { Home } from './screens/home';
import { StyleGuide } from './screens/styleguide';
import { ExampleKeyboardAccessoryView } from './components/examplekeyboardaccessoryview';
import vars from './_vars';

const config = {
  WEBSOCKET_ENDPOINT: undefined,
  GRAPHQL_ENDPOINT: vars.GRAPHQL_ENDPOINT,
  AWS_REGION: vars.AWS_REGION,
  IDENTITY_POOL_ID: vars.IDENTITY_POOL_ID,
  USER_POOL_ID: vars.USER_POOL_ID,
  CLIENT_ID: vars.CLIENT_ID
};

const Tabs = (initialRouteName: string) => createBottomTabNavigator({
  Home: {
    screen: createStackNavigator({ Home: () => <ExampleKeyboardAccessoryView name="home"><Home /></ExampleKeyboardAccessoryView> }),
    navigationOptions: { tabBarIcon: createTabBarIcon('home', 'Home') }
  },
  StyleGuide: {
    screen: createStackNavigator({ StyleGuide: () => <ExampleKeyboardAccessoryView name="sg"><StyleGuide /></ExampleKeyboardAccessoryView> }),
    navigationOptions: { tabBarIcon: createTabBarIcon('brush', 'StyleGuide') }
  },
  Profile: {
    screen: createStackNavigator({ Profile: () => <ExampleKeyboardAccessoryView name="profile"><Profile /></ExampleKeyboardAccessoryView> }),
    navigationOptions: { tabBarIcon: createTabBarIcon('person', 'Profile') }
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
    <AccountProvider
      region={config.AWS_REGION}
      identityPoolId={config.IDENTITY_POOL_ID}
      userPoolId={config.USER_POOL_ID}
      clientId={config.CLIENT_ID}
    >
      <AccountConsumer>{account => {
        const authorization = account && account.user && `Bearer ${account.user.tokens.idToken}` || 'Guest';
        return (
          <ApolloProvider
            authorization={authorization}
            onAuthorizationError={async () => {
              if (account) {
                await account.account.signOut();
                console.log('sign-out');
              }
            }}
            websocketEndpoint={config.WEBSOCKET_ENDPOINT}
            graphqlEndpoint={config.GRAPHQL_ENDPOINT}
          >
            <UI.AssertSingleton fn={App}>
              <Layout />
              {overlays}
            </UI.AssertSingleton>
          </ApolloProvider>
        );
      }}
      </AccountConsumer>
    </AccountProvider>}
  </Mobile.Root>
