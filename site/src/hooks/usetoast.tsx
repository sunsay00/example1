import * as React from 'react';
import * as UI from 'gatsby-theme-core-ui';

export enum ToastType { Info, Success, Error };

export const Toast = (props: {
  duration?: number,
  onClose?: () => void,
  mask?: boolean,
  type: ToastType,
  onAnimationEnd?: () => void,
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
        props.onAnimationEnd && props.onAnimationEnd();
        console.log('END');
        setCurrent(undefined);
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
    <UI.View style={{
      position: 'absolute',
      top: UI.Platform.OS === 'ios' ? 64 : 54,
      left: 0,
      bottom: 0,
      right: 0,
      backgroundColor: 'transparent',
      justifyContent: 'center',
      alignItems: 'center',
    }} pointerEvents={props.mask ? undefined : 'box-none'} >
      <UI.View style={{ backgroundColor: 'transparent' }}>
        <UI.Animated.View style={{ opacity: fadeAnimRef.current }}>
          <UI.View style={{
            alignItems: 'center',
            backgroundColor: 'rgba(0, 0, 0, .8)',
            minWidth: 100,
            borderRadius: 3,
            paddingVertical: 9,
            paddingHorizontal: 15
          }}>
            <UI.Text style={{
              color: 'white',
              fontSize: 15,
              textAlign: 'center'
            }}>{props.children}</UI.Text>
          </UI.View>
        </UI.Animated.View>
      </UI.View>
    </UI.View>
  );
}

type ContextValue = {
  current: JSX.Element | undefined,
  setCurrent: (c: JSX.Element | undefined) => void,
};

const ToastContext = React.createContext<ContextValue>({ current: undefined, setCurrent: _ => console.warn('toast context undefined') });

export const ToastProvider = (props: { children?: React.ReactNode }) => {
  const [current, setCurrent] = React.useState<JSX.Element | undefined>(undefined);

  return (
    <>
      <ToastContext.Provider value={{ current, setCurrent }}>
        <UI.View style={{ flex: 1, height: '100vh' }}>
          {props.children}
          {current && <div style={{ top: 0, left: 0, right: 0, bottom: 0, position: 'fixed' }}>{current}</div>}
        </UI.View>
      </ToastContext.Provider>
      <UI.View>
        {current && <div style={{ top: 0, left: 0, right: 0, bottom: 0, position: 'fixed' }}>{current}</div>}
      </UI.View>, [current]);
    </>
  );
};

export const useToast = () => {
  const { setCurrent } = React.useContext(ToastContext);
  return {
    info: (msg: string) => setCurrent(<Toast type={ToastType.Info}>{msg}</Toast>),
    error: (msg: string) => setCurrent(<Toast type={ToastType.Error}>{msg}</Toast>),
    success: (msg: string) => setCurrent(<Toast type={ToastType.Success}>{msg}</Toast>),
  };
}
