// @ts-check

import { AppRegistry } from 'react-native';
import { App } from './src/app';
import { useScreens } from 'react-native-screens';

useScreens();

AppRegistry.registerComponent('mobile', () => App);
