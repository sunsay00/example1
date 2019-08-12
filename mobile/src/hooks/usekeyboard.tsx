import * as React from 'react';
import { Platform, KeyboardEvent, Keyboard } from 'react-native'

const KeyboardContext = React.createContext({ visible: false, height: 0 });

export const useKeyboard = () => {
  const keyboard = React.useContext(KeyboardContext);
  return keyboard;
}

export const KeyboardProvider = (props: { children?: React.ReactNode }) => {
  const [keyboard, setKeyboard] = React.useState({ visible: false, height: 0 });

  const keyboardWillShow = (e: KeyboardEvent) => {
    setKeyboard({ visible: true, height: e.endCoordinates.height });
  };

  const keyboardWillHide = (e: KeyboardEvent) => {
    setKeyboard({ visible: false, height: e.endCoordinates.height });
  };

  React.useEffect(() => {
    const keyboardShowEvent = Platform.OS === 'ios' ? 'keyboardWillShow' : 'keyboardDidShow';
    const keyboardHideEvent = Platform.OS === 'ios' ? 'keyboardWillHide' : 'keyboardDidHide';
    const keyboardWillShowListener = Keyboard.addListener(keyboardShowEvent, keyboardWillShow);
    const keyboardWillHideListener = Keyboard.addListener(keyboardHideEvent, keyboardWillHide);
    return () => {
      keyboardWillShowListener.remove();
      keyboardWillHideListener.remove();
    }
  }, []);

  return (
    <KeyboardContext.Provider value={keyboard}>
      {props.children}
    </KeyboardContext.Provider>
  );
}

/*
import * as React from 'react';
import { Dimensions, ViewStyle, LayoutChangeEvent, View, Platform, Keyboard, KeyboardEvent, LayoutAnimation, LayoutAnimationConfig, Animated, KeyboardEventEasing } from 'react-native';

const { height, width } = Dimensions.get('window');
const isSafeAreaSupported = Platform.OS === 'ios' && (height > 800 || width > 800);

type AnimationConfig = (duration: number, easint: KeyboardEventEasing) => LayoutAnimationConfig;

const accessoryAnimation = (
  duration: number,
  easing: KeyboardEventEasing,
  animationConfig: AnimationConfig | LayoutAnimationConfig | undefined
): LayoutAnimationConfig => {

  if (animationConfig) {
    if (typeof animationConfig === 'function') {
      return animationConfig(duration, easing);
    }
    return animationConfig;
  }

  if (Platform.OS === 'android') {
    return {
      duration: 200,
      create: {
        duration: 200,
        type: LayoutAnimation.Types.linear,
        property: LayoutAnimation.Properties.opacity
      },
      update: {
        type: LayoutAnimation.Types.linear,
      }
    }
  }

  return LayoutAnimation.create(
    duration,
    LayoutAnimation.Types[easing],
    LayoutAnimation.Properties.opacity,
  )
}

enum KeyboardEventType { Show, Hide };

type ContextValue = { type: KeyboardEventType, event: KeyboardEvent };

const KeyboardContext = React.createContext<ContextValue | undefined>(undefined);

export const KeyboardAccessoryView = (props: {
  style?: ViewStyle,
  avoidKeyboard?: boolean,
  children?: React.ReactNode,
  animateOn?: 'ios' | 'android' | 'all' | 'none',
  hideBoarder?: boolean,
  inSafeAreaView?: boolean,
  visibleOpacity?: number,
  hiddenOpacity?: number,
  bumperHiehgt?: number,
  onKeyboardShowDelay: number | boolean,
  androidAdjustResize: boolean,
  animationConfig?: LayoutAnimationConfig,
  alwaysVisible?: boolean,
}) => {
  const [accessoryHeight, setAccessoryHeight] = React.useState(0);
  const [visibleAccessoryHeight, setVisibleAccessoryHeight] = React.useState(50);
  const [visible, setVisible] = React.useState(false);
  const [keyboardHeight, setKeyboardHeight] = React.useState(0);

  const ctx = React.useContext(KeyboardContext);
  if (!ctx) return <>{props.children}</>;

  const show = (keyboardEvent: KeyboardEvent) => {
    if (!keyboardEvent.endCoordinates) {
      return;
    }

    const keyboardHeight = Platform.select({
      ios: keyboardEvent.endCoordinates.height,
      android: (props.androidAdjustResize || false) ? 0 : keyboardEvent.endCoordinates.height
    });

    //if (Platform.OS === 'ios' || typeof props.onKeyboardShowDelay !== 'number') {
      //keyboardAnimate();
    //} else {
      //setTimeout(() => {
        //keyboardAnimate()
      //}, props.onKeyboardShowDelay);
    //}

    setAccessoryHeight(visibleAccessoryHeight);
    setVisible(true);
    setKeyboardHeight(keyboardHeight);
  }

  const hide = (keyboardEvent: KeyboardEvent) => {
    const animateOn = props.animateOn || 'ios';
    if (animateOn === 'all' || Platform.OS === animateOn) {
      LayoutAnimation.configureNext(
        props.animationConfig || accessoryAnimation(keyboardEvent.duration, keyboardEvent.easing, props.animationConfig)
      );
    }

    setAccessoryHeight(props.alwaysVisible ? visibleAccessoryHeight : 0);
    setVisible(false);
    setKeyboardHeight(0);
  }

  const keyboardAnimate = () => {
    if (animateOn === 'all' || Platform.OS === animateOn) {
      LayoutAnimation.configureNext(
        accessoryAnimation(ctx.event.duration, ctx.event.easing, props.animationConfig)
      );
    }

    setVisible(true);
    setKeyboardHeight(keyboardHeight);
  };

  React.useEffect(() => {
    if (ctx.type == KeyboardEventType.Show) show(ctx.event);
    else if (ctx.type == KeyboardEventType.Hide) hide(ctx.event);
  }, [ctx]);

  const layout = (event: LayoutChangeEvent) => {
    setVisibleAccessoryHeight(event.nativeEvent.layout.height);
    setAccessoryHeight(props.alwaysVisible ? event.nativeEvent.layout.height : 0);
  };

  const avoidKeyboard = props.avoidKeyboard || false;
  const inSafeAreaView = props.inSafeAreaView || false;
  const hideBorder = props.hideBoarder || false;
  const visibleOpacity = props.visibleOpacity || 0;
  const hiddenOpacity = props.hiddenOpacity || 0;
  const bumperHeight = props.bumperHiehgt || 16;
  const animateOn = props.animateOn || 'ios';

  const visibleHeight = accessoryHeight + (avoidKeyboard ? keyboardHeight : 0);
  const applySafeArea = isSafeAreaSupported && inSafeAreaView;

  return (
    <View style={{ height: (visible || props.alwaysVisible ? visibleHeight : 0), flex: 1 }}>
      <View style={[
        {
          position: 'absolute',
          right: 0,
          left: 0,
          backgroundColor: '#EFF0F1',
        },
        !hideBorder && {
          borderTopWidth: 1,
          borderTopColor: 'rgba(0,0,0,0.2)',
        },
        props.style,
        {
          opacity: (visible || props.alwaysVisible ? visibleOpacity : hiddenOpacity),
          bottom: keyboardHeight - bumperHeight - (applySafeArea ? 20 : 0),
          height: accessoryHeight + bumperHeight + (applySafeArea ? (!visible ? 20 : -10) : 0),
        }
      ]}>
        <View style={{ flex: 1 }} onLayout={layout}>
          {props.children}
        </View>
      </View>
    </View>
  );
}

export const KeyboardEventProvider = (props: {
  children?: React.ReactNode,
}) => {

  const [ctx, setCtx] = React.useState<ContextValue | undefined>(undefined);

  React.useEffect(() => {
    const keyboardShowEvent = Platform.OS === 'ios' ? 'keyboardWillShow' : 'keyboardDidShow';
    const keyboardHideEvent = Platform.OS === 'ios' ? 'keyboardWillHide' : 'keyboardDidHide';
    const showListener = Keyboard.addListener(keyboardShowEvent, event => setCtx({ type: KeyboardEventType.Show, event }));
    const hideListener = Keyboard.addListener(keyboardHideEvent, event => setCtx({ type: KeyboardEventType.Hide, event }));
    return () => {
      showListener.remove();
      hideListener.remove();
    };
  }, []);

  if (!ctx) {
    return <>{props.children}</>;
  } else {
    return (
      <KeyboardContext.Provider value={ctx}>
        {props.children}
      </KeyboardContext.Provider>
    );
  }
}
*/