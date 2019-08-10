import * as React from 'react';
import { ViewStyle, TouchableWithoutFeedback } from 'react-native';
import * as UI from 'core-ui';
import IonIcon from 'react-native-ionicons';

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
  style?: ViewStyle,
  size?: UI.Size,
  name: keyof typeof Icons,
  color?: string,
  disabled?: boolean,
  onPress?: () => void,
}) => {
  const slop = 16;
  return (
    <TouchableWithoutFeedback disabled={props.disabled || !props.onPress} style={props.style} onPress={props.onPress} hitSlop={{ left: slop, top: slop, bottom: slop, right: slop }}>
      <UI.Text
        style={{
          ..._styles.common,
          fontFamily: 'FontAwesome',
          backgroundColor: UI.Colors.transparency,
          fontSize: _styles.fontSize[props.size || 'lg'],
          color: props.color || UI.rgba(UI.Colors.black, props.onPress ? 1 : .8),
          opacity: props.disabled ? .25 : 1,
        }}>
        {Icons[props.name]}
      </UI.Text>
    </TouchableWithoutFeedback>
  );
}

export const Icon2 = (props: UI.Icon2Props) => {
  const slop = 16;
  const fontSize = _styles.fontSize[props.size || 'lg'];
  return (
    <UI.TouchableWithoutFeedback
      disabled={props.disabled || !props.onPress}
      style={props.style} onPress={props.onPress}
      hitSlop={{ left: slop, top: slop, bottom: slop, right: slop }}>
      <UI.View style={{ width: fontSize, height: fontSize }}>
        <IonIcon style={{ color: props.color, fontSize }} name={props.name} />
      </UI.View>
    </UI.TouchableWithoutFeedback>
  );
}