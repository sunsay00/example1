import * as React from 'react';
import * as UI from 'core-ui';
import { useTopViewStack } from '../hooks/usetopviewstack';

type Options = { cancelable: boolean };

type ButtonOption = { text: string, onPress?: () => void };

type Opts = {
  title: string,
  message?: string,
  options?: Options,
  buttons: ButtonOption[]
}

let _alert: ((opts: Opts) => void) | undefined;

export const AlertProvider = (props: {
  children?: React.ReactNode,
}) => {
  const [visible, setVisible] = React.useState(false);
  const [opts, setOpts] = React.useState<Opts>({ title: '', buttons: [] });
  const optsRef = React.useRef(opts);
  optsRef.current = opts;
  const stack = useTopViewStack();

  React.useEffect(() => {
    _alert = (opts: Opts) => {
      setVisible(true);
      setOpts(opts);
    };
    return () => _alert = undefined;
  }, []);

  React.useEffect(() => stack.register(AlertProvider, {
    animationType: 'slide',
    onDismiss: () => setVisible(false)
  }), []);

  const onButtonPress = (onPress?: () => void) => () => {
    setVisible(false);
    onPress && onPress();
  };

  React.useEffect(() => {
    if (visible) {
      stack.push(AlertProvider,
        <UI.View style={{ flex: 1, height: '100vh' }}>
          <div style={{ top: 0, left: 0, right: 0, bottom: 0, position: 'fixed' }}>
            <UI.View style={{ ...UI.StyleSheet.absoluteFillObject, alignItems: 'center', justifyContent: 'center' }}>
              <UI.View style={{
                overflow: 'hidden',
                backgroundColor: UI.Colors.white,
                padding: 32,
                shadowColor: UI.rgba('#000000', .65),
                shadowOffset: { width: 0, height: 16 },
                shadowRadius: 32,
              }}>
                <UI.Header3>{optsRef.current.title}</UI.Header3>
                <UI.Text>{optsRef.current.message}</UI.Text>
                <UI.Spacer size="lg" />
                <UI.View style={{ alignSelf: 'flex-end', flexDirection: 'row', flex: 1 }}>
                  {optsRef.current.buttons.map(({ text, onPress }, i) =>
                    <UI.Link style={{ paddingLeft: 32 }} key={i} onPress={onButtonPress(onPress)}>{text}</UI.Link>)}
                </UI.View>
              </UI.View>
            </UI.View>
          </div>
        </UI.View>);
    } else {
      stack.pop(AlertProvider);
    }
  }, [visible]);

  return <>{props.children}</>;
}

export const Alert = {
  alert: (title: string, message?: string, buttons: ButtonOption[] = [{ text: 'OK' }], options?: Options) => {
    if (!_alert) {
      console.warn('alert provider not initialized');
    } else {
      _alert({ title, message, buttons, options });
    }
  }
};