import * as React from 'react';
import { Modal } from '../components/modal';
import * as UI from 'core-ui';

type AnimationType = 'slide' | 'fade' | 'none';

type Options = {
  animationType?: AnimationType,
  onDismiss?: () => void,
}

type ElemEntry = {
  name: string,
  elem: JSX.Element | null,
}

type ContextValue = {
  top: JSX.Element | null,
  register: (fn: Function, opts?: Options) => () => void,
  push: (fn: Function, c: JSX.Element | null) => void,
  pop: (fn: Function) => void,
};

const TopViewStackContext = React.createContext<ContextValue>({
  top: null,
  register: _ => () => console.warn('invalid topviewstack context'),
  push: () => console.warn('invalid topviewstack context'),
  pop: _ => console.warn('invalid topviewstack context'),
});

export const useTopViewStack = () => React.useContext(TopViewStackContext);

export const TopViewStackProvider = (props: {
  style?: UI.ViewStyle,
  children?: React.ReactNode,
  renderWrapper?: (modal: JSX.Element) => JSX.Element,
}) => {
  const wrapper = props.renderWrapper || (x => x);
  const [stack, setStack] = React.useState<{ ents: ElemEntry[], prev: ElemEntry[] }>({ ents: [], prev: [] });
  const [handlers, setHandlers] = React.useState<{ onShow?: () => void, onDismiss?: () => void }>({});
  const [current, setCurrent] = React.useState<{ busy: boolean, visible: boolean, ent: ElemEntry | null }>({ busy: false, visible: false, ent: null });
  const [opts, setOpts] = React.useState<{ [_: string]: Options | undefined }>({});

  const register = (fn: Function, opts?: Options) => {
    setOpts(o => ({ ...o, [fn.name]: opts }));
    return () => {
      setOpts(o => {
        const ret = { ...o };
        delete ret[fn.name];
        return ret;
      });
    }
  };

  const hide = (onHidden: () => void) => {
    setHandlers({
      onShow: () => setCurrent(p => ({ ...p, busy: false, })),
      onDismiss: () => { setCurrent(p => ({ ...p, busy: false })); onHidden(); }
    });
    setCurrent(p => ({ ...p, busy: true, visible: false, ent: null }));
  }

  const show = (ent: ElemEntry | null, onShown: () => void) => {
    setHandlers({
      onShow: () => { setCurrent(p => ({ ...p, busy: false })); onShown(); },
      onDismiss: () => setCurrent(p => ({ ...p, busy: false })),
    });
    setCurrent(p => ({ ...p, busy: true, visible: true, ent }));
  }

  React.useEffect(() => {
    if (current.busy) return;
    if (stack.ents.length == 0) {
      if (current.visible)
        hide(() => { });
    } else {
      const last = stack.ents[stack.ents.length - 1];
      const pushed = stack.ents.length > stack.prev.length;
      if (pushed) {
        if (current.visible) {
          hide(() => {
            show(last, () => { });
          });
        } else {
          show(last, () => { });
        }
      } else {
        hide(() => {
          show(last, () => { });
        });
      }
    }
  }, [stack.ents.length]);

  const push = (fn: Function, elem: JSX.Element | null) => {
    if (current.busy) return;
    setStack(s => ({ ents: [...s.ents, { name: fn.name, elem }], prev: s.ents }));
  }

  const pop = (fn: Function) => {
    if (current.busy) return;
    if (stack.ents.length == 0) return;
    if (current.ent)
      console.assert(fn.name == current.ent.name);
    setStack(s => {
      const ents = [...s.ents];
      ents.pop();
      return { ...s, ents };
    });
  }

  const top = current.ent || { name: '', elem: null };
  const o = opts[top.name];
  const onShow = handlers.onShow || (() => setCurrent(p => ({ ...p, busy: false })));
  const onDismiss = handlers.onDismiss || (() => setCurrent(p => ({ ...p, busy: false })));
  const onUserDismiss = o && o.onDismiss || (() => { });

  return (
    <>
      <TopViewStackContext.Provider value={{ top: top.elem, push, pop, register }}>
        {props.children}
      </TopViewStackContext.Provider>
      <div
        style={current.visible ? {
          ...UI.StyleSheet.absoluteFillObject,
          //display: current.visible ? 'visible' : 'none',
          position: 'fixed',
          backgroundColor: UI.rgba('#000000', .65)
        } : {}}
      >
        <Modal animationType={o && o.animationType || 'none'}
          transparent
          visible={current.visible}
          onShow={onShow}
          onDismiss={() => {
            onDismiss();
            onUserDismiss();
          }}
        >
          <UI.View
            key={top.name}>
            {top.elem ? wrapper(top.elem) : top.elem}
          </UI.View>
        </Modal>
      </div>
    </>
  );
}
