import * as React from 'react';
import * as UI from '@inf/core-ui';

type Opts = {
  title: string,
  message?: string,
  buttons: UI.AlertButton[]
  options?: UI.AlertOptions,
}

let _alert: ((opts: Opts) => void) | undefined;

export const AlertProvider = (props: {
  children?: React.ReactNode,
}) => {
  const [translateY, setVisible] = UI.useSlideUpAnimation(false);
  const { display, dismiss, requestDismissal } = UI.useTopViewStack(AlertProvider, {
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
        <UI.View style={{ ...UI.StyleSheet.absoluteFillObject, alignItems: 'center', justifyContent: 'center' }}>
          <UI.TouchableWithoutFeedback>
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
          </UI.TouchableWithoutFeedback>
        </UI.View>
      );
      setVisible(true);
    };
    return () => _alert = undefined;
  }, []);

  return <>{props.children}</>;
}

export const Alert = {
  alert: (title: string, message?: string, buttons: UI.AlertButton[] = [{ text: 'OK' }], options?: UI.AlertOptions) => {
    if (!_alert) {
      console.warn('alert provider not initialized');
    } else {
      _alert({ title, message, buttons, options });
    }
  }
};
