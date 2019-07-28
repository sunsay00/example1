import * as React from 'react';
import { StyleSheet } from 'react-native';
import { View, computeTextStyle } from 'core-ui';

export const WebTable = (props: { children?: React.ReactNode }) =>
  <View style={{ flexDirection: 'row', marginBottom: 16 }}>
    <table cellPadding={16} cellSpacing={0} style={{ borderColor: '#00000016' }} {...props} />
  </View>

export const WebTableBody = ({ children, ...props }: { children?: React.ReactNode }) =>
  <tbody {...props}>{children && React.Children.map(children, (child: any, index: number) => {
    return child && React.cloneElement(child, { ...child.props, style: { ...child.props.style }, index });
  })}</tbody>

export const WebTableRow = ({ index, ...props }: { index: number, children?: React.ReactNode }) =>
  <tr style={{ backgroundColor: index >= 0 && index % 2 == 0 ? '#00000008' : undefined }}>
    {React.isValidElement(props.children) && React.Children.map(props.children, (child: React.ReactChild) =>
      React.isValidElement(child) && React.cloneElement(child, { ...child.props }) || child) || props.children}</tr>

export const WebTableData = (props: any) => <td style={{
  borderBottomWidth: StyleSheet.hairlineWidth, borderBottomStyle: 'solid',
  borderColor: '#00000016',
  ...computeTextStyle({ size: 'xs', weight: 'thin' }),
}} {...props} />

export const WebTableHeader = (props: any) => <th style={{
  borderBottomWidth: StyleSheet.hairlineWidth, borderBottomStyle: 'solid',
  borderColor: '#00000016',
  ...computeTextStyle({ size: 'sm', weight: 'thin' }),
}} {...props} />
