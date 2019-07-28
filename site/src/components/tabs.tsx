import * as React from 'react';
import { sg, ViewStyle, TouchableOpacity, View, Text } from 'gatsby-theme-core-ui';
import { has } from 'common';

export const Tabs = (props: {
  initalIndex: number,
  children?: React.ReactNode
  style?: ViewStyle,
  hideFrame?: boolean,
}) => {
  const children: React.ReactNode[] = props.children && React.Children.map(props.children, (child: React.ReactNode) =>
    React.isValidElement(child) && has(child.props, 'title', 'string') && child.props.title ? child : null).filter(child => !!child) || [];
  const titles = children.map(child => React.isValidElement(child) && has(child.props, 'title', 'string') ? child.props.title : '');
  const [selectedIndex, setSelectedIndex] = React.useState(props.initalIndex > titles.length ? titles.length - 1 : props.initalIndex);
  return (
    <View style={props.style}>
      <View style={{ flexDirection: 'row' }}>
        {titles.map((title, i) =>
          <View key={i} style={i == selectedIndex ?
            {
              borderTopLeftRadius: 8,
              borderTopRightRadius: 8,
              borderColor: sg.rgba(sg.colors.black, .25),
              borderLeftWidth: 1,
              borderTopWidth: 1,
              borderRightWidth: 1,
            } : {
              borderBottomWidth: 1,
              borderColor: sg.rgba(sg.colors.black, .25),
            }}>
            <TouchableOpacity style={{
              paddingVertical: 16,
              paddingHorizontal: 24,
            }} onPress={() => setSelectedIndex(i)}>
              <Text size="md" color={i == selectedIndex ? sg.colors.black : sg.rgba(sg.colors.black, .5)}>{title}</Text>
            </TouchableOpacity>
          </View>
        )}
        <View style={{
          flex: 1,
          paddingVertical: 8,
          paddingHorizontal: 24,
          borderBottomWidth: 1,
          borderColor: sg.rgba(sg.colors.black, .25),
        }} />
      </View>
      <View style={{
        padding: 16,
        ...!props.hideFrame && {
          borderLeftWidth: 1,
          borderRightWidth: 1,
          borderBottomWidth: 1,
          borderColor: sg.rgba(sg.colors.black, .25),
          borderBottomLeftRadius: 8,
          borderBottomRightRadius: 8,
        } || undefined
      }}>{children[selectedIndex]}</View>
    </View>
  );
}

export const Tab = (props: { title: string, children?: React.ReactNode }) =>
  <>{props.children}</>