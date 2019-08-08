import * as React from 'react';
import { Fragment } from 'react';
import { SafeAreaView, StatusBar, } from 'react-native';
import * as UI from 'core-ui';
import { createAppContainer, createStackNavigator } from "react-navigation";

const HomeScreen = (props: {}) =>
  <Fragment>
    <UI.View style={{ flex: 1, alignItems: "center", justifyContent: "center" }}>
      <StatusBar barStyle="dark-content" />
      <SafeAreaView>
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
      </SafeAreaView>
    </UI.View>
  </Fragment>

export const Navigator = createStackNavigator({
  Home: {
    screen: HomeScreen
  }
});

export const App = createAppContainer(Navigator);