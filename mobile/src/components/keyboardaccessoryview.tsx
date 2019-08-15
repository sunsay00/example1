import * as React from 'react';
import { StyleSheet, Easing, Animated, ViewStyle, LayoutChangeEvent, Platform, StatusBar, View, Dimensions, TextInput, UIManager, findNodeHandle } from 'react-native';
import { useKeyboard } from 'mobile-ui';
import { useAnimation, useTopViewStack } from 'core-ui';

const DURATION = Platform.OS == 'android' ? 0 : 250;

export const KeyboardAccessoryView = (props: {
  accessoryElement: React.ReactNode,
  accessoryHeight: number,
  children?: React.ReactNode,
  style?: ViewStyle,
}) => {
  const windowHeight = Dimensions.get('window').height - (Platform.OS == 'android' ? (StatusBar.currentHeight || 0) : 0);
  const [accessoryHeight, setAccessoryHeight] = React.useState(0);
  const [viewHeight, setViewHeight] = React.useState(0);
  const innerRef = React.useRef<View | null>(null);
  const { visible, height: _height } = useKeyboard();
  const height = Platform.OS == 'android' ? 0 : _height;
  const [inputBottom, setInputBottom] = React.useState(accessoryHeight);
  const fieldId = TextInput.State.currentlyFocusedField();
  const accessoryTop = windowHeight - height - accessoryHeight;
  const [offset, setOffset] = useAnimation(0, { duration: DURATION });
  const [bottom, setBottom] = React.useState(0);
  const viewRef = React.useRef<View | null>(null);
  const [accessoryOffset, setAccessoryOffset] = useAnimation(0, { duration: DURATION, easing: Easing.out(Easing.quad) });
  const [paddingBottom, setPaddingBottom] = React.useState(0);

  React.useEffect(() => {
    let next = 0;
    if (Platform.OS == 'android') {
      next += windowHeight - accessoryHeight - _height;
    } else {
      next += viewHeight - height;
      next += bottom - accessoryHeight;
    }
    setAccessoryOffset({ toValue: next });
  }, [viewHeight, accessoryHeight, bottom, visible]);

  React.useEffect(() => {
    setOffset({ toValue: (visible && inputBottom > accessoryTop) ? accessoryTop - inputBottom : 0 });
  }, [visible, inputBottom, accessoryTop]);

  React.useEffect(() => {
    if (visible) setAccessoryHeight(props.accessoryHeight);
    else setAccessoryHeight(0);
  }, [visible]);

  React.useLayoutEffect(() => {
    if (fieldId) {
      if (innerRef.current) {
        const ancestorHandle = findNodeHandle(innerRef.current);
        if (ancestorHandle) {
          UIManager.measureLayout(fieldId, ancestorHandle, () => { }, (_l, _t, _w, h) =>
            UIManager.measure(fieldId, (_x, _y, _w, _h, _px, py) =>
              setInputBottom(h + py)));
        }
      }
    } else {
      setInputBottom(accessoryHeight);
    }
  }, [fieldId, innerRef.current]);

  if (Platform.OS == 'android') {
    const { display } = useTopViewStack(KeyboardAccessoryView, { disableBackground: true });
    React.useEffect(() => {
      display(() =>
        <Accessory
          style={props.style}
          accessoryHeight={accessoryHeight}
          visible={visible}
          accessoryOffset={accessoryOffset}
          height={height}
        >{props.accessoryElement}</Accessory>
      )
    }, [props.style, accessoryHeight, visible, accessoryOffset, height, props.accessoryElement]);
  }

  const layout = (e: LayoutChangeEvent) => {
    if (viewRef.current) {
      viewRef.current.measure((x, y, w, h, px, py) => {
        const top = py - y;
        setBottom(windowHeight - h - top);
      });
      setViewHeight(e.nativeEvent.layout.height);
    }
  };

  if (Platform.OS == 'android') {
    React.useEffect(() => {
      const to = setTimeout(() => {
        setPaddingBottom(accessoryHeight);
      }, 20);
      return () => clearTimeout(to);
    }, [accessoryHeight]);
  }

  return (
    <>
      <View ref={r => viewRef.current = r} style={{ flex: 1, ...StyleSheet.absoluteFillObject }}>
        <Animated.View
          onLayout={layout}
          style={{
            transform: [{ translateY: offset }],
            flex: 1, ...StyleSheet.absoluteFillObject,
            bottom: 0,
          }}>
          <View
            onLayout={() => null}
            ref={r => innerRef.current = r}
            style={{ flex: 1, paddingBottom }}>
            {props.children}
          </View>
        </Animated.View>
        {Platform.OS != 'android' &&
          <Accessory
            style={props.style}
            accessoryHeight={accessoryHeight}
            visible={visible}
            accessoryOffset={accessoryOffset}
            height={height}
          >{props.accessoryElement}</Accessory> || null}
      </View>
    </>
  );
}

const Accessory = (props: {
  style?: ViewStyle,
  accessoryHeight: number,
  visible: boolean,
  accessoryOffset: Animated.AnimatedInterpolation,
  height: number,
  alwaysVisible?: boolean,
  children?: React.ReactNode
}) => {
  return (
    <Animated.View
      onStartShouldSetResponder={() => props.visible}
      style={{
        ...props.style,
        transform: [{ translateY: props.accessoryOffset }],
        flex: 1, ...StyleSheet.absoluteFillObject,
        height: props.accessoryHeight,
      }}
    >
      <View style={{
        flex: 1, ...StyleSheet.absoluteFillObject,
        backgroundColor: props.style && props.style.backgroundColor,
        height: props.height + props.accessoryHeight,
      }} />
      {(props.alwaysVisible || (!props.alwaysVisible && props.visible)) && props.children || null}
    </Animated.View>
  );
}