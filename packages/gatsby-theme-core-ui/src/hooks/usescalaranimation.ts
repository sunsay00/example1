import { useAnimation, AnimationResult } from './useanimation';

export const useScalarAnimation = (initialValue: number, duration: number = 300): AnimationResult<number> => {
  const [value, setValue] = useAnimation(initialValue);
  return [
    value,
    (toValue: number, onEnd?: () => void) => {
      setValue({ toValue, duration }, onEnd);
    }
  ];
};