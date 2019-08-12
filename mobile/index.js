// @ts-check

import { AppRegistry } from 'react-native';
import { App } from './src/app';
import { useScreens } from 'react-native-screens';

//console.ignoredYellowBox = ['Warning: Failed propType: SceneView'];
console.disableYellowBox = true;

useScreens();

AppRegistry.registerComponent('mobile', () => App);
