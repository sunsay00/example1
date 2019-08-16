import * as React from 'react';
import { Platform, KeyboardEvent, Keyboard } from 'react-native';

type ContextValue = { visible: boolean, height: number };

const KeyboardContext = React.createContext<ContextValue | undefined>(undefined);

export const KeyboardConsumer = KeyboardContext.Consumer;

export const useKeyboard = () => {
  const keyboard = React.useContext(KeyboardContext);
  if (!keyboard) throw new Error('invalid keyboard context');
  return keyboard;
}

export const KeyboardProvider = (props: { children?: React.ReactNode }) => {
  const [keyboard, setKeyboard] = React.useState({ visible: false, height: 0 });

  const keyboardWillShow = (e: KeyboardEvent) => {
    setKeyboard({ visible: true, height: e.endCoordinates.height });
  };

  const keyboardWillHide = (e: KeyboardEvent) => {
    setKeyboard({ visible: false, height: 0 });
  };

  React.useEffect(() => {
    const keyboardShowEvent = Platform.OS === 'ios' ? 'keyboardWillShow' : 'keyboardDidShow';
    const keyboardHideEvent = Platform.OS === 'ios' ? 'keyboardWillHide' : 'keyboardDidHide';
    const keyboardWillShowListener = Keyboard.addListener(keyboardShowEvent, keyboardWillShow);
    const keyboardWillHideListener = Keyboard.addListener(keyboardHideEvent, keyboardWillHide);
    return () => {
      keyboardWillShowListener.remove();
      keyboardWillHideListener.remove();
    }
  }, []);

  return (
    <KeyboardContext.Provider value={keyboard}>
      {props.children}
    </KeyboardContext.Provider>
  );
}