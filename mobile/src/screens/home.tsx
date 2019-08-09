import * as React from 'react';
import * as UI from 'core-ui';
import { useNavigation, useNavigationOptions } from '../hooks/usenavigation';

export const Home = () => {
  const nav = useNavigation();
  useNavigationOptions({
    title: 'Home'
  });
  return (
    <UI.ScrollView>
      <UI.View style={{ flex: 1, alignItems: 'center', justifyContent: 'center' }}>
        <UI.StatusBar barStyle='dark-content' />
        <UI.SafeAreaView>
          <UI.Accent>123456789</UI.Accent>
          <UI.Header1>Infinage Styleguide</UI.Header1>
          <UI.Header2>Table of Contents</UI.Header2>
          <UI.Header2>Components</UI.Header2>
          <UI.Header4>Accent</UI.Header4>
          <UI.Header2>123456789</UI.Header2>
          <UI.Text weight="black" numberOfLines={1}>123456789</UI.Text>
          <UI.Text weight="bold" numberOfLines={1}>123456789</UI.Text>
          <UI.Text weight="medium" numberOfLines={1}>123456789</UI.Text>
          <UI.Text weight="light" numberOfLines={1}>123456789</UI.Text>
          <UI.Text weight="thin" numberOfLines={1}>123456789</UI.Text>
          <UI.View style={{ flex: 1, justifyContent: 'center', alignItems: 'center' }}>
            <UI.Button onPress={nav.openDrawer}>Open Drawer</UI.Button>
          </UI.View>
        </UI.SafeAreaView>
      </UI.View>
    </UI.ScrollView>
  );
}
