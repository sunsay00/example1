import * as React from 'react';
import * as UI from 'core-ui';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';

export const Icons = UI.Icons;

const _styles = {
  common: {
    fontFamily: 'FontAwesome',
  },
  fontSize: {
    xxs: 12,
    xs: 16,
    sm: 22,
    md: 26,
    lg: 32,
    xl: 46,
    xxl: 60
  }
};

export const Icon = (props: {
  style?: UI.ViewStyle,
  size?: UI.Size,
  name: keyof typeof Icons,
  color?: string,
  disabled?: boolean,
  onPress?: () => void,
}) => {
  const slop = 16;
  const size = _styles.fontSize[props.size || 'lg'];
  const icon = Icons[props.name];
  return (
    <UI.TouchableWithoutFeedback disabled={props.disabled || !props.onPress} style={props.style} onPress={props.onPress} hitSlop={{ left: slop, top: slop, bottom: slop, right: slop }}>
      <UI.View style={{ width: size, height: size }}>
        <FontAwesomeIcon
          style={{
            backgroundColor: UI.Colors.transparency,
            fontSize: _styles.fontSize[props.size || 'lg'],
            color: props.color || UI.rgba(UI.Colors.black, props.onPress ? 1 : .8),
            opacity: props.disabled ? .25 : 1,
          }}
          icon={icon} />
      </UI.View>
    </UI.TouchableWithoutFeedback>
  );
}