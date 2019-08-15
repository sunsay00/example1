import * as React from 'react';
import { useState, useContext, useEffect } from 'react';
import { Easing, Animated } from 'react-native';
import * as Nav from 'react-navigation';
import * as UI from 'core-ui';
import * as Mobile from 'mobile-ui';
import { NavHeaderButton } from '../components/navheaderbutton';

export * from 'react-navigation';

export function useNav<S>(): Nav.NavigationScreenProp<S & Nav.NavigationRoute> {
  return useContext(Nav.NavigationContext as any);
}

export function useNavParam<T extends keyof Nav.NavigationParams>(
  paramName: T
) {
  return useNav().getParam(paramName);
}

export function useNavState() {
  return useNav().state;
}

export function useNavKey() {
  return useNav().state.key;
}

export function useNavEvents(handleEvt: Nav.NavigationEventCallback) {
  const navigation = useNav();
  useEffect(
    () => {
      const subsA = navigation.addListener(
        'action' as any // TODO should we remove it? it's not in the published typedefs
        , handleEvt);
      const subsWF = navigation.addListener('willFocus', handleEvt);
      const subsDF = navigation.addListener('didFocus', handleEvt);
      const subsWB = navigation.addListener('willBlur', handleEvt);
      const subsDB = navigation.addListener('didBlur', handleEvt);
      return () => {
        subsA.remove();
        subsWF.remove();
        subsDF.remove();
        subsWB.remove();
        subsDB.remove();
      };
    },
    // For TODO consideration: If the events are tied to the navigation object and the key
    // identifies the nav object, then we should probably pass [navigation.state.key] here, to
    // make sure react doesn't needlessly detach and re-attach this effect. In practice this
    // seems to cause troubles
    undefined
    // [navigation.state.key]
  );
}

const emptyFocusState = {
  isFocused: false,
  isBlurring: false,
  isBlurred: false,
  isFocusing: false,
};
const didFocusState = { ...emptyFocusState, isFocused: true };
const willBlurState = { ...emptyFocusState, isBlurring: true };
const didBlurState = { ...emptyFocusState, isBlurred: true };
const willFocusState = { ...emptyFocusState, isFocusing: true };
const getInitialFocusState = (isFocused: boolean) =>
  isFocused ? didFocusState : didBlurState;
function focusStateOfEvent(eventName: Nav.EventType) {
  switch (eventName) {
    case 'didFocus':
      return didFocusState;
    case 'willFocus':
      return willFocusState;
    case 'willBlur':
      return willBlurState;
    case 'didBlur':
      return didBlurState;
    default:
      return null;
  }
}

export function useFocusState() {
  const navigation = useNav();
  const isFocused = navigation.isFocused();
  const [focusState, setFocusState] = useState(getInitialFocusState(isFocused));
  function handleEvt(e: Nav.NavigationEventPayload) {
    const newState = focusStateOfEvent(e.type);
    newState && setFocusState(newState);
  }
  useNavEvents(handleEvt);
  return focusState;
}

export const createStackNavigator = (
  routeConfigMap: Nav.NavigationRouteConfigMap,
  stackConfig?: Nav.StackNavigatorConfig
): Nav.NavigationContainer => {
  return Nav.createStackNavigator(routeConfigMap, {
    ...stackConfig,
    transitionConfig: () => ({
      transitionSpec: {
        duration: 400,
        easing: Easing.out(Easing.poly(4)),
        timing: Animated.timing,
        useNativeDriver: true,
      },
      screenInterpolator: (sceneProps: Nav.NavigationTransitionProps) => {
        const { layout, position, scene } = sceneProps
        const thisSceneIndex = scene.index
        const width = layout.initWidth
        const translateX = position.interpolate({
          inputRange: [thisSceneIndex - 1, thisSceneIndex],
          outputRange: [width, 0],
        });
        return { transform: [{ translateX }] }
      }
    }),
    defaultNavigationOptions: (props: Nav.NavigationScreenConfigProps): Nav.NavigationStackScreenOptions => ({
      title: props.navigation.getParam('title'),
      headerLeft: props.navigation.getParam('headerLeft'),
      headerRight: props.navigation.getParam('headerRight'),
      headerTitleStyle: {
        color: UI.Colors.green,
        fontFamily: UI.Fonts.sansSerif.weightProps.medium.name,
      },
      headerBackImage:
        <Nav.NavigationContext.Consumer>{nav =>
          <NavHeaderButton prefixIconName="arrow-back" onPress={() => nav.goBack()} />}
        </Nav.NavigationContext.Consumer>,
      headerBackTitle: null,
      ...(stackConfig ? stackConfig.defaultNavigationOptions : {})
    }),
  });
}

export const useNavOptions = (opts?: {
  title?: string,
  headerLeft?: React.ReactNode,
  headerRight?: React.ReactNode,
}) => {
  const nav = useNav();
  useEffect(() => {
    nav.setParams(opts || {});
    return () => {
      nav.setParams({});
    };
  }, []);
}

export const createNavWrapper = <State extends Nav.NavigationState, Options extends {}, Props extends {}>(
  renderWrapper: (props: { children?: React.ReactNode }) => React.ReactNode,
  Wrapped: Nav.NavigationContainer) => {
  return class extends React.Component<Nav.NavigationContainerProps & Nav.NavigationNavigatorProps<Options, State>, Props> {
    static router: Nav.NavigationRouter<State, Options> = Wrapped.router;
    static navigationOptions?: Nav.NavigationScreenConfig<Options> = Wrapped.navigationOptions;
    render() {
      return <>{renderWrapper({ children: <Wrapped {...this.props} /> })}</>;
    }
  }
}

const BottomTabBar = (props: Nav.BottomTabBarProps) => {
  const { visible } = Mobile.useKeyboard();
  return visible ? <UI.View /> : <Nav.BottomTabBar {...props} />;
}

export const createBottomTabNavigator = <State extends Nav.NavigationState, Options extends {}, Props extends {}>(
  routeConfigMap: Nav.NavigationRouteConfigMap,
  drawConfig?: Nav.BottomTabNavigatorConfig
) => {
  const windowHeight = UI.Dimensions.get('window').height - (UI.Platform.OS == 'android' ? (UI.StatusBar.currentHeight || 0) : 0);
  const Wrapped = Nav.createBottomTabNavigator(routeConfigMap, {
    ...drawConfig,
    tabBarComponent: BottomTabBar,
    tabBarOptions: {
      ...drawConfig && drawConfig.tabBarOptions,
      activeTintColor: UI.Colors.green,
      inactiveTintColor: UI.rgba(UI.Colors.black, .5),
      labelStyle: {
        fontFamily: UI.Fonts.sansSerif.weightProps.medium.name,
      },
    },
  });
  if (UI.Platform.OS == 'android') {
    return class extends React.Component<Nav.NavigationContainerProps & Nav.NavigationNavigatorProps<Options, State>, Props> {
      static router = Wrapped.router;
      static navigationOptions = Wrapped.navigationOptions;
      render() {
        return (
          <Mobile.KeyboardConsumer>{ctx => {
            if (!ctx) return null;
            return (
              <UI.View style={{ flex: 1, minHeight: windowHeight - ctx.height }}>
                <Wrapped {...this.props} />
              </UI.View>
            );
          }}
          </Mobile.KeyboardConsumer>
        );
      }
    };
  } else {
    return Wrapped;
  }
}
