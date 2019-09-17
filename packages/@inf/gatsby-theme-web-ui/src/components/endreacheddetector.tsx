import * as React from 'react';
import * as UI from '@inf/core-ui';
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
  }, [props.onEndReached]);
  return (
    <div ref={ref => divRef.current = ref}>
      {props.children}
      {props.onEndReached &&
        <UI.View style={{ paddingTop: 16, paddingBottom: 32 }}>
          <UI.ActivityIndicator size="small" />
        </UI.View>}
    </div>
  );
}