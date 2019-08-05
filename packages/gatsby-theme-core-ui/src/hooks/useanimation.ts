import * as React from 'react';
import * as UI from 'core-ui';

export type AnimationResult<T> = [UI.Animated.AnimatedInterpolation, (t: T, onEnd?: () => void) => void]

export const useAnimation = (initialValue: number): AnimationResult<UI.Animated.TimingAnimationConfig> => {
  const valueRef = React.useRef(new UI.Animated.Value(initialValue));
  const animRef = React.useRef<UI.Animated.CompositeAnimation | undefined>();
  return [
    valueRef.current,
    (config: UI.Animated.TimingAnimationConfig, onEnd?: () => void) => {
      animRef.current && animRef.current.stop();
      animRef.current = UI.Animated.timing(valueRef.current, config);
      animRef.current.start(onEnd);
    }
  ];
}
