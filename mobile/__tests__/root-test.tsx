import * as React from 'react';
import { View as mockView } from 'react-native';
import { App } from '../src/app';

// Note: test renderer must be required after react-native.
import * as renderer from 'react-test-renderer';

jest.mock('react-navigation', () => ({
  createAppContainer: () => mockView,
  createStackNavigator: () => undefined,
}));

it('renders screen', () => {
  renderer.create(<App />);
});
