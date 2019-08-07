import * as React from 'react';
import * as UI from 'core-ui';
import { useTopViewStack } from '../hooks/usetopviewstack';

type ContextValue = {
  setCurrent: (c: React.ReactNode) => void,
};

export enum ToastType { Info, Success, Warn, Error };

const ToastContext = React.createContext<ContextValue>({
  setCurrent: _ => console.warn('toast context undefined')
});

export const ToastProvider = (props: {
  children?: React.ReactNode,
}) => {
  const { display, dismiss } = useTopViewStack(ToastProvider, { disableBackground: true });
  const setCurrent = (node: React.ReactNode) => {
    if (node) display(() => node);
    else dismiss();
  }
  return (
    <ToastContext.Provider value={{ setCurrent }}>
      {props.children}
    </ToastContext.Provider>
  );
};

const Toast = (props: {
  duration?: number,
  onClose?: () => void,
  mask?: boolean,
  type: ToastType,
  onEnd?: () => void,
  children: string
}) => {
  const { setCurrent } = React.useContext(ToastContext);
  const fadeAnimRef = React.useRef(new UI.Animated.Value(0));
  const animRef = React.useRef<UI.Animated.CompositeAnimation>();

  React.useEffect(() => {
    const duration = props.duration || 2;
    const animArr = [
      UI.Animated.timing(fadeAnimRef.current, { toValue: 1, duration: 200 }),
      UI.Animated.delay(duration * 1000),
    ];
    if (duration > 0) {
      animArr.push(
        UI.Animated.timing(fadeAnimRef.current, { toValue: 0, duration: 200 }),
      );
    }
    animRef.current = UI.Animated.sequence(animArr);
    animRef.current.start(() => {
      if (duration > 0) {
        animRef.current = undefined;
        props.onClose && props.onClose();
        props.onEnd && props.onEnd();
        setCurrent(null);
      }
    });
    return () => {
      if (animRef.current != undefined) {
        animRef.current.stop();
        animRef.current = undefined;
      }
    }
  }, []);

  return (
    <UI.View style={{ ...UI.StyleSheet.absoluteFillObject, alignItems: 'center', justifyContent: 'center' }}>
      <UI.TouchableWithoutFeedback>
        <UI.Animated.View style={{ opacity: fadeAnimRef.current }}>
          <UI.View style={{
            alignItems: 'center',
            backgroundColor: 'rgba(0, 0, 0, .8)',
            minWidth: 100,
            borderRadius: 3,
            paddingVertical: 9,
            paddingHorizontal: 15
          }}>
            <UI.Text style={{ color: 'white', textAlign: 'center' }}>
              {props.children}
            </UI.Text>
          </UI.View>
        </UI.Animated.View>
      </UI.TouchableWithoutFeedback>
    </UI.View>
  );
}

export const useToast = () => {
  const { setCurrent } = React.useContext(ToastContext);
  return {
    info: (msg: string, onEnd?: () => void) => {
      console.log(`Toast-Info: ${msg}`)
      setCurrent(<Toast type={ToastType.Info} onEnd={onEnd}>{msg}</Toast>);
    },
    warn: (msg: string, onEnd?: () => void) => {
      console.warn(`Toast-Warn: ${msg}`)
      setCurrent(<Toast type={ToastType.Warn} onEnd={onEnd}>{msg}</Toast>);
    },
    error: (msg: string, onEnd?: () => void) => {
      console.error(`Toast-Error: ${msg}`)
      setCurrent(<Toast type={ToastType.Error} onEnd={onEnd}>{msg}</Toast>);
    },
    success: (msg: string, onEnd?: () => void) => {
      console.log(`Toast-Success: ${msg}`)
      setCurrent(<Toast type={ToastType.Success} onEnd={onEnd}>{msg}</Toast>);
    },
  };
}
