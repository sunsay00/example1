import * as React from 'react';
import * as UI from 'core-ui';

export const NavHeaderButton = (props: {
  prefixIconName?: UI.IconName,
  suffixIconName?: UI.IconName,
  children?: string,
  onPress?: () => void,
}) => {
  const { loading } = UI.useLoading(NavHeaderButton);
  return (
    <UI.TouchableOpacity onPress={props.onPress}>
      <UI.View style={{ paddingHorizontal: 16, flexDirection: 'row', alignItems: 'center' }}>
        {props.prefixIconName && <UI.Icon size="md" disabled={loading} name={props.prefixIconName} style={{ marginRight: 8 }} />}
        {props.children && <UI.Text style={{ color: loading ? UI.rgba(UI.Colors.black, .5) : UI.Colors.green }}>{props.children}</UI.Text>}
        {props.suffixIconName && <UI.Icon size="md" disabled={loading} name={props.suffixIconName} style={{ marginLeft: 8 }} />}
      </UI.View>
    </UI.TouchableOpacity>
  );
}