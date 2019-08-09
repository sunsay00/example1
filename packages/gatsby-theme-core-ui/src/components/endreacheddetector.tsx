import * as React from 'react';
import { debounce } from 'debounce';

export const EndReachedDetector = (props: {
  onEndReached?: () => void,
  onEndReachedThreshold?: number,
  children?: React.ReactNode
}) => {
  const divRef = React.useRef<HTMLDivElement | null>(null);
  React.useEffect(() => {
    const fn = debounce((e: Event) => {
      if (!divRef.current) return;
      const { bottom } = divRef.current.getBoundingClientRect();
      const threshold = props.onEndReachedThreshold || 0;
      if (bottom * threshold < window.innerHeight)
        props.onEndReached && props.onEndReached();
    }, 500);
    window.addEventListener('scroll', fn);
    return () => window.removeEventListener('scroll', fn);
  }, []);
  return (
    <div ref={ref => divRef.current = ref}>
      {props.children}
    </div>
  );
}