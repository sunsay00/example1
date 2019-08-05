import * as React from 'react';
import * as UI from 'core-ui';
import { useScalarAnimation } from '../hooks/usescalaranimation';
import { useBodyScrollLocker } from '../hooks/usebodyscrolllocker';

type TopStackOptions = {
  onDismissRequest?: () => void
  disableBackground?: boolean,
}

type OverlayParams = {
  dismissRequested?: boolean,
  nodeFn?: () => React.ReactNode,
  opts: TopStackOptions
}

type ContextValue = {
  register: (fn: Function) => () => void,
  display: (fn: Function, nodeFn: () => React.ReactNode, params?: TopStackOptions) => void,
  dismiss: (fn: Function) => void,
  requestDismissal: (fn: Function) => void,
};

const TopViewStackContext = React.createContext<ContextValue>({
  register: _ => () => console.warn('invalid topviewstack context'),
  display: _ => console.warn('invalid topviewstack context'),
  dismiss: _ => console.warn('invalid topviewstack context'),
  requestDismissal: _ => console.warn('invalid topviewstack context'),
});

export const useTopViewStack = (fn: Function, opts?: TopStackOptions) => {
  const stack = React.useContext(TopViewStackContext);
  const display = (nodeFn: () => React.ReactNode) => stack.display(fn, nodeFn, opts);
  const dismiss = () => stack.dismiss(fn);
  const requestDismissal = () => stack.requestDismissal(fn);
  React.useEffect(stack.register(fn), []);
  return { display, dismiss, requestDismissal };
}

export const TopViewStackProvider = (props: {
  style?: UI.ViewStyle,
  children?: React.ReactNode,
  renderWrapper?: (modal: React.ReactNode) => JSX.Element,
}) => {
  const wrapper = props.renderWrapper || (x => x);
  const [_overlays, setOverlays] = React.useState<{ key: string, params: OverlayParams }[]>([]);
  const overlaysRef = React.useRef(_overlays);
  overlaysRef.current = _overlays;

  const setOverlay = (fn: Function, paramsFn: (prev: OverlayParams) => OverlayParams) =>
    setOverlays(p => p.map(i => i.key == fn.name ? { ...i, params: paramsFn(i.params) } : i));

  const register = (fn: Function) => {
    if (!overlaysRef.current.find(o => o.key == fn.name)) {
      const params: OverlayParams = { opts: {} };
      setOverlays(p => {
        if (p.find(i => i.key == fn.name)) {
          return p.map(i => i.key == fn.name ? { ...i, params } : i);
        } else {
          return [{ key: fn.name, params }, ...p];
        }
      });
    }
    return () => setOverlays(p => p.filter(i => i.key != fn.name));
  };

  const display = (fn: Function, nodeFn: () => React.ReactNode, opts?: TopStackOptions) => {
    setOverlay(fn, p => ({ ...p, dismissRequested: false, nodeFn, opts: opts || {} }));
  }

  const dismiss = (fn: Function) => {
    setOverlay(fn, p => ({ ...p, nodeFn: undefined }));
  }

  const requestDismissal = (fn: Function) => {
    //const enabledOverlays = overlaysRef.current.filter(o => !!o.params.nodeFn && o.key != fn.name);
    //const lastEnabledOverlay = enabledOverlays.length && enabledOverlays[enabledOverlays.length - 1] || undefined;
    //const enabled = lastEnabledOverlay != undefined;
    //!enabled &&
    setOverlay(fn, p => ({ ...p, dismissRequested: true }));
  }

  const enabledOverlays = overlaysRef.current.filter(o => !!o.params.nodeFn);
  const lastEnabledOverlay = enabledOverlays.length && enabledOverlays[enabledOverlays.length - 1] || undefined;
  const dismissRequested = lastEnabledOverlay && lastEnabledOverlay.params.dismissRequested || false;
  const enabled = lastEnabledOverlay != undefined;
  const onDismissRequest = lastEnabledOverlay && lastEnabledOverlay.params.opts.onDismissRequest || undefined;
  const disableBackground = enabledOverlays.filter(o => !o.params.opts.disableBackground).length == 0;
  const bgIndex = enabledOverlays.reduce((a, o, i) => o.params.opts.disableBackground || false ? a : i, 0);

  useBodyScrollLocker(enabled);

  const [opacity, setOpacity] = useScalarAnimation(0);
  React.useEffect(() => {
    setOpacity(!disableBackground && !dismissRequested ? 1 : 0);
  }, [dismissRequested, disableBackground]);

  return (
    <TopViewStackContext.Provider value={{ register, display, dismiss, requestDismissal }}>
      {props.children ? wrapper(<>{props.children}</>) : props.children}
      <UI.TouchableWithoutFeedback disabled={!onDismissRequest} onPress={onDismissRequest} style={enabled ? {
        top: UI.Platform.select({ ios: 64, android: 54, web: 0 }),
        left: 0, right: 0, bottom: 0, position: 'absolute',
      } : {}}>
        <UI.View>
          {wrapper(enabledOverlays.map((o, i) =>
            <div key={i} style={{ top: 0, left: 0, right: 0, bottom: 0, position: 'fixed' }}>
              {i == bgIndex &&
                <UI.Animated.View style={{ ...UI.StyleSheet.absoluteFillObject, backgroundColor: UI.rgba('#000000', .65), opacity }} />}
              <UI.View style={{ ...UI.StyleSheet.absoluteFillObject, alignItems: 'center', justifyContent: 'center' }}>
                <UI.TouchableWithoutFeedback>
                  {o.params.nodeFn!()}
                </UI.TouchableWithoutFeedback>
              </UI.View>
            </div>
          ))}
        </UI.View>
      </UI.TouchableWithoutFeedback>
    </TopViewStackContext.Provider>
  );
}
