import * as React from 'react';
import * as UI from '@inf/core-ui';
import { has } from '@inf/common';

export const Tabs = (props: {
  initalIndex: number,
  children?: React.ReactNode
  style?: UI.ViewStyle,
  hideFrame?: boolean,
}) => {
  const children: React.ReactNode[] = props.children && React.Children.map(props.children, (child: React.ReactNode) =>
    React.isValidElement(child) && has(child.props, 'title', 'string') && child.props.title ? child : null).filter(child => !!child) || [];
  const titles = children.map(child => React.isValidElement(child) && has(child.props, 'title', 'string') ? child.props.title : '');
  const [selectedIndex, setSelectedIndex] = React.useState(props.initalIndex > titles.length ? titles.length - 1 : props.initalIndex);
  return (
    <UI.View style={props.style}>
      <UI.View style={{ flexDirection: 'row' }}>
        {titles.map((title, i) =>
          <UI.View key={i} style={i == selectedIndex ?
            {
              borderTopLeftRadius: 8,
              borderTopRightRadius: 8,
              borderColor: UI.rgba(UI.Colors.black, .25),
              borderLeftWidth: 1,
              borderTopWidth: 1,
              borderRightWidth: 1,
            } : {
              borderBottomWidth: 1,
              borderColor: UI.rgba(UI.Colors.black, .25),
            }}>
            <UI.TouchableOpacity style={{
              paddingVertical: 16,
              paddingHorizontal: 24,
            }} onPress={() => setSelectedIndex(i)}>
              <UI.Text size="md" color={i == selectedIndex ? UI.Colors.black : UI.rgba(UI.Colors.black, .5)}>{title}</UI.Text>
            </UI.TouchableOpacity>
          </UI.View>
        )}
        <UI.View style={{
          flex: 1,
          paddingVertical: 8,
          paddingHorizontal: 24,
          borderBottomWidth: 1,
          borderColor: UI.rgba(UI.Colors.black, .25),
        }} />
      </UI.View>
      <UI.View style={{
        padding: 16,
        ...!props.hideFrame && {
          borderLeftWidth: 1,
          borderRightWidth: 1,
          borderBottomWidth: 1,
          borderColor: UI.rgba(UI.Colors.black, .25),
          borderBottomLeftRadius: 8,
          borderBottomRightRadius: 8,
        } || undefined
      }}>{children[selectedIndex]}</UI.View>
    </UI.View>
  );
}

export const Tab = (props: { title: string, children?: React.ReactNode }) =>
  <>{props.children}</>