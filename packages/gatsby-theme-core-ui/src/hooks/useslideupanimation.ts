import * as UI from 'core-ui';
import { useAnimation, AnimationResult } from './useanimation';

export const useSlideUpAnimation = (initialVisible: boolean): AnimationResult<boolean> => {
  const [value, setValue] = useAnimation(initialVisible ? 1 : 0);
  const windowHeight = UI.Dimensions.get('window').height;
  return [
    value.interpolate({
      inputRange: [0, 1],
      outputRange: [windowHeight, 0],
      extrapolate: 'clamp',
    }),
    (visible: boolean, onEnd?: () => void) =>
      setValue({
        toValue: visible ? 1 : 0,
        easing: UI.Easing.out(UI.Easing.poly(4)),
        duration: 300,
      }, onEnd)
  ];
}
