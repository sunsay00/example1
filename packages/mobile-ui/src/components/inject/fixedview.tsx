import * as React from 'react';
import * as UI from 'core-ui';

export const FixedView = (props: { style?: UI.ViewStyle, children?: React.ReactNode }) => {
  const ref = React.useRef<UI.View | null>(null);
  const width = UI.Dimensions.get('screen').width;
  const height = UI.Dimensions.get('screen').height;
  const [pos, setPos] = React.useState<{ px: Number, py: number }>({ px: 0, py: 0 });

  React.useEffect(() => {
    if (ref.current)
      ref.current.measure((_x, _y, _w, _h, px, py) => setPos({ px, py }));
  }, [ref.current]);

  return (
    <UI.View ref={r => ref.current = r} style={{ ...props.style, position: 'absolute', left: -pos.px, top: -pos.py, width, height }}>
      {props.children}
    </UI.View>
  );
}
