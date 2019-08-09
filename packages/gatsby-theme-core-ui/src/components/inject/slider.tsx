import * as React from 'react';
import { SliderProps, LayoutChangeEvent, ViewStyle, GestureResponderEvent, Animated, Easing, PanResponder, PanResponderInstance, StyleSheet, View, I18nManager } from 'react-native';
import applyNativeMethods from 'react-native-web/dist/modules/applyNativeMethods';

// polyfilling react-native-web is missing slider implementation

const getWebSlider = () => {
  class Rect {
    x: number;
    y: number;
    width: number;
    height: number;
    constructor(x: number, y: number, width: number, height: number) {
      this.x = x;
      this.y = y;
      this.width = width;
      this.height = height;
    }
    containsPoint(x: number, y: number) {
      return (x >= this.x && y >= this.y && x <= this.x + this.width && y <= this.y + this.height);
    }
  }

  const getOffset = () => {
    if (document.documentElement && document.documentElement.scrollTop) {
      // Explorer 6 Strict
      return {
        x: document.documentElement.scrollLeft,
        y: document.documentElement.scrollTop,
      };
    }
    // all other Explorers
    return { x: document.body.scrollLeft, y: document.body.scrollTop };
  }


  const TRACK_SIZE = 4;
  const THUMB_SIZE = 20;
  const THUMB_TOUCH_SIZE = 40;

  const DEFAULT_ANIMATION_CONFIGS = {
    spring: {
      friction: 7,
      tension: 100,
    },
    timing: {
      duration: 150,
      easing: Easing.inOut(Easing.ease),
      delay: 0,
    },
  };

  type SliderState = {
    containerSize: { width: number, height: number },
    trackSize: { width: number, height: number },
    thumbSize: { width: number, height: number },
    allMeasured: boolean
  };

  class _Slider extends React.Component<SliderProps, SliderState> {
    _panResponder: PanResponderInstance;
    _previousLeft: number = 0;
    _store: {
      [key: string]: { width: number, height: number, [x: string]: any },
    } = {};

    static defaultProps = {
      value: 0,
      minimumValue: 0,
      maximumValue: 0,
      step: 0,
      minimumTrackTintColor: '#009688',
      maximumTrackTintColor: '#939393',
      debugTouchArea: false,
      animationType: 'timing',
    } as SliderProps;

    state = {
      containerSize: { width: 0, height: 0 },
      trackSize: { width: 0, height: 0 },
      thumbSize: { width: 0, height: 0 },
      allMeasured: false,
    } as SliderState;

    _value: Animated.Value;

    constructor(props: SliderProps) {
      super(props);
      this._value = new Animated.Value(props.value || 0);
      this._panResponder = PanResponder.create({
        onStartShouldSetPanResponder: this._handleStartShouldSetPanResponder,
        onMoveShouldSetPanResponder: this._handleMoveShouldSetPanResponder,
        onPanResponderGrant: this._handlePanResponderGrant,
        onPanResponderMove: this._handlePanResponderMove,
        onPanResponderRelease: this._handlePanResponderEnd,
        onPanResponderTerminationRequest: this._handlePanResponderRequestEnd,
        onPanResponderTerminate: this._handlePanResponderEnd,
      });
    }

    private _curvalue: number = 0;

    componentDidMount() {
      this._value.addListener(({ value }) => this._curvalue = value);
      if (this.props.value != undefined && this.props.value !== this._curvalue)
        this._value.setValue(this.props.value);
    }

    componentWillUnmount() {
      this._value.removeAllListeners();
    }

    componentWillReceiveProps(nextProps: SliderProps) {
      const newValue = nextProps.value;
      if (this.props.value !== newValue) {
        this._setCurrentValue(newValue || 0);
      }
    }

    render() {
      const { minimumValue, maximumValue, minimumTrackTintColor, maximumTrackTintColor, style, ...other } = this.props as SliderProps;
      const { containerSize, thumbSize, allMeasured } = this.state;
      const thumbLeft = this._value.interpolate({
        inputRange: [minimumValue || 0, maximumValue || 0],
        outputRange: I18nManager.isRTL
          ? [0, -(containerSize.width - thumbSize.width)]
          : [0, containerSize.width - thumbSize.width],
      });
      const minimumTrackWidth = this._value.interpolate({
        inputRange: [minimumValue || 0, maximumValue || 0],
        outputRange: [0, containerSize.width - thumbSize.width],
      });

      const valueVisibleStyle = {} as { opacity: undefined | number };
      if (!allMeasured) {
        valueVisibleStyle.opacity = 0;
      }

      const minimumTrackStyle = {
        position: 'absolute',
        width: Animated.add(minimumTrackWidth, thumbSize.width / 2),
        backgroundColor: minimumTrackTintColor,
        ...valueVisibleStyle,
      };

      const touchOverflowStyle = this._getTouchOverflowStyle();

      return (
        <View style={{ flexDirection: 'column', flex: 1 }}>
          <View
            {...other}
            onLayout={this._measureContainer}
            style={[defaultStyles.container, style]} >
            <View
              onLayout={this._measureTrack}
              style={
                [
                  { backgroundColor: maximumTrackTintColor },
                  defaultStyles.track,
                ]}
            />
            <Animated.View style={[defaultStyles.track, minimumTrackStyle]} />
            <Animated.View
              onLayout={this._measureThumb}
              style={
                [
                  { backgroundColor: minimumTrackTintColor },
                  defaultStyles.thumb,
                  {
                    transform: [{ translateX: thumbLeft }, { translateY: 0 }],
                    ...valueVisibleStyle,
                  },
                ]}
            />
            <View
              style={[defaultStyles.touchArea, touchOverflowStyle]}
              {...this._panResponder.panHandlers}>
            </View>
          </View>
        </View>
      );
    }

    _getPropsForComponentUpdate(props: SliderProps) {
      /* eslint-disable */
      const { value, onValueChange, onSlidingComplete, style, ...otherProps } = props;

      return otherProps;
      /* eslint-enable */
    }

    _handleStartShouldSetPanResponder = (e: GestureResponderEvent, gestureState: Object): boolean => {
      // Should we become active when the user presses down on the thumb?
      return this._thumbHitTest(e, gestureState);
    };

    _handleMoveShouldSetPanResponder(/*e: Object, gestureState: Object*/): boolean {
      // Should we become active when the user moves a touch over the thumb?
      return false;
    }

    _handlePanResponderGrant = (/*e: Object, gestureState: Object*/) => {
      this._previousLeft = this._getThumbLeft(this._getCurrentValue());
      //this.props.onSlidingStart && this.props.onSlidingStart(this._getCurrentValue());
    };

    _handlePanResponderMove = (e: Object, gestureState: { dx: number }) => {
      if (this.props.disabled) {
        return;
      }

      this._setCurrentValue(this._getValue(gestureState));
      this.props.onValueChange && this.props.onValueChange(this._getCurrentValue());
    };

    _handlePanResponderRequestEnd(e: Object, gestureState: Object) {
      // Should we allow another component to take over this pan?
      return false;
    }

    _handlePanResponderEnd = (e: Object, gestureState: { dx: number }) => {
      if (this.props.disabled)
        return;
      this._setCurrentValue(this._getValue(gestureState));
      this.props.onSlidingComplete && this.props.onSlidingComplete(this._getCurrentValue());
    };

    _measureContainer = (x: LayoutChangeEvent) => {
      this._handleMeasure('containerSize', x);
    };

    _measureTrack = (x: LayoutChangeEvent) => {
      this._handleMeasure('trackSize', x);
    };

    _measureThumb = (x: LayoutChangeEvent) => {
      this._handleMeasure('thumbSize', x);
    };

    _handleMeasure = (name: string, x: LayoutChangeEvent) => {
      const { width, height } = x.nativeEvent.layout;
      const size = { width: width, height: height };

      const currentSize = this._store[name];
      if (
        currentSize &&
        width === currentSize.width &&
        height === currentSize.height
      ) {
        return;
      }
      this._store[name] = size;

      const store = this._store;
      if (store.containerSize && store.trackSize && store.thumbSize) {
        this.setState({
          containerSize: store.containerSize,
          trackSize: store.trackSize,
          thumbSize: store.thumbSize,
          allMeasured: true,
        });
      }
    };

    _getRatio = (value: number) => {
      const minimumValue = this.props.minimumValue || 0;
      const maximumValue = this.props.maximumValue || 0;
      return (
        (value - minimumValue) /
        (maximumValue - minimumValue)
      );
    };

    _getThumbLeft = (value: number) => {
      const nonRtlRatio = this._getRatio(value);
      const ratio = I18nManager.isRTL ? 1 - nonRtlRatio : nonRtlRatio;
      return (
        ratio * (this.state.containerSize.width - this.state.thumbSize.width)
      );
    };

    _getValue = (gestureState: { dx: number }) => {
      const length = this.state.containerSize.width - this.state.thumbSize.width;
      const thumbLeft = this._previousLeft + gestureState.dx;

      const nonRtlRatio = thumbLeft / length;
      const ratio = I18nManager.isRTL ? 1 - nonRtlRatio : nonRtlRatio;

      const minimumValue = this.props.minimumValue || 0;
      const maximumValue = this.props.maximumValue || 0;

      if (this.props.step) {
        return Math.max(
          minimumValue,
          Math.min(
            maximumValue, minimumValue +
            Math.round((ratio * (maximumValue - minimumValue)) / this.props.step) *
            this.props.step,
          ),
        );
      }
      return Math.max(
        minimumValue,
        Math.min(maximumValue, ratio * (maximumValue - minimumValue) + minimumValue),
      );
    };

    _getCurrentValue = () => this._curvalue;

    _setCurrentValue = (value: number) => {
      this._value.setValue(value);
    };

    _setCurrentValueAnimated = (value: number) => {
      const animationType = 'timing';
      const animationConfig = { ...DEFAULT_ANIMATION_CONFIGS[animationType], toValue: value };
      Animated[animationType](this._value, animationConfig).start();
    };

    _getTouchOverflowSize = () => {
      const { state } = this;

      const size = { width: 0, height: 0 };
      if (state.allMeasured === true) {
        size.width = Math.max(0, THUMB_TOUCH_SIZE - state.thumbSize.width);
        size.height = Math.max(0, THUMB_TOUCH_SIZE - state.containerSize.height);
      }
      return size;
    };

    _getTouchOverflowStyle = () => {
      const { width, height } = this._getTouchOverflowSize();

      const touchOverflowStyle = {} as ViewStyle;
      if (width !== undefined && height !== undefined) {
        const verticalMargin = -height / 2;
        touchOverflowStyle.marginTop = verticalMargin;
        touchOverflowStyle.marginBottom = verticalMargin;

        const horizontalMargin = -width / 2;
        touchOverflowStyle.marginLeft = horizontalMargin;
        touchOverflowStyle.marginRight = horizontalMargin;
      }

      return touchOverflowStyle;
    };

    _thumbHitTest = ({ nativeEvent }: GestureResponderEvent, gestureState: Object) => {
      const thumbTouchRect = this._getThumbTouchRect();
      const offset = getOffset();
      return thumbTouchRect.containsPoint(
        nativeEvent.locationX - offset.x,
        nativeEvent.locationY - offset.y,
      );
    };

    _getThumbTouchRect = () => {
      const { state } = this;
      const touchOverflowSize = this._getTouchOverflowSize();

      return new Rect(
        touchOverflowSize.width / 2 +
        this._getThumbLeft(this._getCurrentValue()) +
        (state.thumbSize.width - THUMB_TOUCH_SIZE) / 2,
        touchOverflowSize.height / 2 +
        (state.containerSize.height - THUMB_TOUCH_SIZE) / 2,
        THUMB_TOUCH_SIZE,
        THUMB_TOUCH_SIZE,
      );
    };
  }

  const defaultStyles = StyleSheet.create({
    container: {
      height: 40,
      justifyContent: 'center',
    },
    track: {
      height: TRACK_SIZE,
      borderRadius: TRACK_SIZE / 2,
    },
    thumb: {
      position: 'absolute',
      width: THUMB_SIZE,
      height: THUMB_SIZE,
      borderRadius: THUMB_SIZE / 2,
    },
    touchArea: {
      position: 'absolute',
      backgroundColor: 'transparent',
      top: 0,
      left: 0,
      right: 0,
      bottom: 0,
    },
    debugThumbTouchArea: {
      position: 'absolute',
      backgroundColor: 'green',
      opacity: 0.5,
    },
  });

  return applyNativeMethods(_Slider);
}

export const Slider = getWebSlider();