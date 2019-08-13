import * as React from 'react';
import * as UI from 'core-ui';
import { KeyboardAccessoryView } from './keyboardaccessoryview';

const ExampleKeyboardAccessory = () => {
  const toast = UI.useToast();
  return (
    <UI.View style={{
      flex: 1,
      flexDirection: 'row',
      borderTopWidth: UI.StyleSheet.hairlineWidth,
      borderTopColor: UI.rgba(UI.Colors.black, .3),
      justifyContent: 'center', alignItems: 'center',
    }}>
      <UI.Button size="sm" onPress={() => toast.info('hello world')}>Toast</UI.Button>
      <UI.Spacer />
      <UI.Button size="sm" onPress={() => UI.Alert.alert('hello world')}>Alert</UI.Button>
      <UI.Spacer />
      <UI.Button size="sm" onPress={() => console.log('hello world')}>Console</UI.Button>
    </UI.View>
  );
}

export const ExampleKeyboardAccessoryView = (props: { children?: React.ReactNode }) =>
  <KeyboardAccessoryView
    accessoryElement={<ExampleKeyboardAccessory />}
    style={{ backgroundColor: 'white' }}
    //alwaysVisible
    accessoryHeight={56}
  >
    {props.children}
  </KeyboardAccessoryView>
