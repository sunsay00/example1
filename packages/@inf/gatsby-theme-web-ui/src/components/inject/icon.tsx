import * as React from 'react';
import * as UI from '@inf/core-ui';

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
  if (props.onPress) {
    return (
      <UI.TouchableWithoutFeedback
        disabled={props.disabled} onPress={props.onPress}
        hitSlop={{ left: slop, top: slop, bottom: slop, right: slop }}>
        <UI.View style={{ height: fontSize, ...props.style }}>
          {
            // @ts-ignore
            <ion-icon style={{ color: props.color || (props.disabled ? UI.rgba(UI.Colors.black, .5) : UI.Colors.green), fontSize }} name={props.name} />
          }
        </UI.View>
      </UI.TouchableWithoutFeedback>
    );
  } else {
    return (
      <UI.View style={{ height: fontSize, ...props.style }}>
        {
          // @ts-ignore
          <ion-icon style={{ color: props.color || (props.disabled ? UI.rgba(UI.Colors.black, .5) : UI.Colors.green), fontSize }} name={props.name} />
        }
      </UI.View>
    );
  }
}