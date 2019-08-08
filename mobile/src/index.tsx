import * as React from 'react';
import { Fragment } from 'react';
import { SafeAreaView, StyleSheet, Text, StatusBar, } from 'react-native';
import * as UI from 'core-ui';

export const App = () => {
  return (
    <Fragment>
      <StatusBar barStyle="dark-content" />
      <SafeAreaView>
        <UI.Accent>123456789</UI.Accent>
        <UI.Header1>Infinage Labs</UI.Header1>
      </SafeAreaView>
    </Fragment>
  );
};