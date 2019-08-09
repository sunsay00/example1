import * as React from 'react';
import { Component, useState, useContext, useEffect } from 'react';
import { Easing, Animated } from 'react-native';
import {
  NavigationTransitionProps, createStackNavigator as createStackNavigatorRN,
  NavigationScreenConfigProps, NavigationStackScreenOptions, NavigationRouteConfigMap,
  StackNavigatorConfig, NavigationContext, NavigationScreenProp, EventType,
  NavigationRoute, NavigationParams, NavigationEventCallback, NavigationEventPayload,
  NavigationContainer, NavigationContainerProps, NavigationNavigatorProps, NavigationRouter, NavigationScreenConfig, NavigationState
} from 'react-navigation';

export * from 'react-navigation';

export function useNavigation<S>(): NavigationScreenProp<S & NavigationRoute> {
  return useContext(NavigationContext as any);
}

export function useNavigationParam<T extends keyof NavigationParams>(
  paramName: T
) {
  return useNavigation().getParam(paramName);
}

export function useNavigationState() {
  return useNavigation().state;
}

export function useNavigationKey() {
  return useNavigation().state.key;
}

export function useNavigationEvents(handleEvt: NavigationEventCallback) {
  const navigation = useNavigation();
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
function focusStateOfEvent(eventName: EventType) {
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
  const navigation = useNavigation();
  const isFocused = navigation.isFocused();
  const [focusState, setFocusState] = useState(getInitialFocusState(isFocused));
  function handleEvt(e: NavigationEventPayload) {
    const newState = focusStateOfEvent(e.type);
    newState && setFocusState(newState);
  }
  useNavigationEvents(handleEvt);
  return focusState;
}

export const createStackNavigator = (
  routeConfigMap: NavigationRouteConfigMap,
  stackConfig?: StackNavigatorConfig
): NavigationContainer => {
  return createStackNavigatorRN(routeConfigMap, {
    transitionConfig: () => ({
      transitionSpec: {
        duration: 400,
        easing: Easing.out(Easing.poly(4)),
        timing: Animated.timing,
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
      }
    }),
    defaultNavigationOptions: (props: NavigationScreenConfigProps): NavigationStackScreenOptions => ({
      title: props.navigation.getParam('title'),
      headerLeft: props.navigation.getParam('headerLeft'),
      headerRight: props.navigation.getParam('headerRight'),
    }),
    ...stackConfig
  });
}

export const useNavigationOptions = (opts: {
  title?: string,
  headerLeft?: React.ReactNode,
  headerRight?: React.ReactNode,
}) => {
  const nav = useNavigation();
  useEffect(() => {
    nav.setParams(opts);
    return () => {
      nav.setParams({});
    };
  }, []);
}

export const createNavWrapper = <State extends NavigationState, Options extends {}, Props extends {}>(
  renderWrapper: (children?: React.ReactNode) => React.ReactNode,
  Wrapped: NavigationContainer) => {
  return class extends Component<NavigationContainerProps & NavigationNavigatorProps<Options, State>, Props> {
    static router: NavigationRouter<State, Options> = Wrapped.router;
    static navigationOptions?: NavigationScreenConfig<Options> = Wrapped.navigationOptions;
    render() {
      return <>{renderWrapper(<Wrapped {...this.props} />)}</>;
    }
  }
}
