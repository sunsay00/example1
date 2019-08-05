import * as React from 'react';
import * as UI from 'core-ui';
import { useTopViewStack } from '../hooks/usetopviewstack';
import { useSlideUpAnimation } from '../hooks/useslideupanimation';

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
  const [translateY, setVisible] = useSlideUpAnimation(false);
  const { display, dismiss, requestDismissal } = useTopViewStack(AlertProvider, {
    onDismissRequest: () => {
      requestDismissal();
      setVisible(false, () =>
        dismiss());
    }
  });

  const onButtonPress = (onPress?: () => void) => () => {
    requestDismissal();
    setVisible(false, () => {
      dismiss();
      onPress && onPress();
    });
  };

  React.useEffect(() => {
    _alert = (opts: Opts) => {
      display(() =>
        <UI.Animated.View style={{ transform: [{ translateY }] }}>
          <UI.View style={{
            overflow: 'hidden',
            backgroundColor: UI.Colors.white,
            padding: 32,
            shadowColor: UI.rgba('#000000', .65),
            shadowOffset: { width: 0, height: 16 },
            shadowRadius: 32,
          }}>
            <UI.Header3>{opts.title}</UI.Header3>
            <UI.Text>{opts.message}</UI.Text>
            <UI.Spacer size="lg" />
            <UI.View style={{ alignSelf: 'flex-end', flexDirection: 'row', flex: 1 }}>
              {opts.buttons.map(({ text, onPress }, i) =>
                <UI.Link style={{ paddingLeft: 32 }} key={i} onPress={onButtonPress(onPress)}>{text}</UI.Link>)}
            </UI.View>
          </UI.View>
        </UI.Animated.View>
      );
      setVisible(true);
    };
    return () => _alert = undefined;
  }, []);

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