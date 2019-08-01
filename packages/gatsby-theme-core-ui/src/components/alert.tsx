import * as React from 'react';
import * as UI from 'core-ui';

type Options = { cancelable: boolean };

type ButtonOption = { text: string, onPress?: () => void };

let _alert: ((title: string, message: string | undefined, buttons: ButtonOption[], options: Options | undefined) => void) | undefined;

export const AlertProvider = (props: { children?: React.ReactNode }) => {
  const [title, setTitle] = React.useState('');
  const [message, setMessage] = React.useState<string>();
  const [options, setOptions] = React.useState<Options>();
  const [visible, setVisible] = React.useState(false);
  const [buttons, setButtons] = React.useState<ButtonOption[]>([]);
  const scrollPosRef = React.useRef({ left: 0, top: 0 });

  const onButtonPress = (onPress?: () => void) => () => {
    setVisible(false);
    onPress && onPress();
  };

  React.useEffect(() => {
    _alert = (title: string, message: string | undefined, buttons: ButtonOption[], options: Options | undefined) => {
      setVisible(true);
      setTitle(title);
      setMessage(message);
      setButtons(buttons);
      setOptions(options);
    };
    return () => _alert = undefined;
  }, []);

  React.useEffect(() => {
    if (visible) {
      // lock body scrolling
      scrollPosRef.current.left = document.documentElement.scrollLeft || document.body.scrollLeft;
      scrollPosRef.current.top = document.documentElement.scrollTop || document.body.scrollTop;
      document.body.style.position = 'fixed';
      document.body.style.width = '100%';
      document.body.style.overflowY = 'scroll';
      document.body.style.top = `-${scrollPosRef.current.top}px`;
    } else {
      // unlock body scolling
      document.body.style.position = 'unset';
      document.body.style.width = 'unset';
      document.body.style.overflowY = 'unset';
      window.scrollTo(scrollPosRef.current.left, scrollPosRef.current.top);
    }
  }, [visible]);

  return (
    <>
      {props.children}
      {visible &&
        <UI.View style={{ flex: 1, height: '100vh' }}>
          <div style={{ top: 0, left: 0, right: 0, bottom: 0, position: 'fixed' }}>
            <UI.View style={{ ...UI.StyleSheet.absoluteFillObject, alignItems: 'center', justifyContent: 'center', backgroundColor: UI.rgba('#000000', .65) }}>
              <UI.View style={{ overflow: 'hidden', backgroundColor: UI.Colors.white, paddingVertical: 16, paddingHorizontal: 32 }}>
                <UI.Header3>{title}</UI.Header3>
                <UI.Text>{message}</UI.Text>
                <UI.Spacer />
                <UI.View style={{ alignSelf: 'flex-end', flexDirection: 'row', flex: 1 }}>
                  {buttons.map(({ text, onPress }, i) =>
                    <UI.Link key={i} onPress={onButtonPress(onPress)}>{text}</UI.Link>)}
                </UI.View>
              </UI.View>
            </UI.View>
          </div>
        </UI.View>
      }
    </>
  );
}

export const Alert = {
  alert: (title: string, message?: string, buttons: ButtonOption[] = [{ text: 'OK' }], options?: Options) => {
    if (!_alert) {
      console.warn('alert provider not initialized');
    } else {
      _alert(title, message, buttons, options);
    }
  }
};