import * as React from 'react';
import * as UI from 'core-ui';

const fontSizes = {
  xxs: 12,
  xs: 16,
  sm: 22,
  md: 26,
  lg: 32,
  xl: 46,
  xxl: 60
};

export const Icon = (props: UI.IconProps) => {
  const slop = 16;
  const fontSize = fontSizes[props.size || 'lg'];
  return (
    <UI.TouchableWithoutFeedback
      disabled={props.disabled || !props.onPress}
      style={props.style} onPress={props.onPress}
      hitSlop={{ left: slop, top: slop, bottom: slop, right: slop }}>
      <UI.View style={{ width: fontSize, height: fontSize }}>
        {
          // @ts-ignore
          <ion-icon style={{ color: props.color, fontSize }} name={props.name} />
        }
      </UI.View>
    </UI.TouchableWithoutFeedback>
  );
}