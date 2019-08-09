import * as React from 'react';
import * as UI from 'core-ui';
import { useTopViewStack } from '../hooks/usetopviewstack';
import { useScalarAnimation } from '../hooks/usescalaranimation';

type ContextValue = {
  current: React.ReactNode,
  setCurrent: (c: React.ReactNode) => void,
};

const ModalContext = React.createContext<ContextValue | undefined>(undefined);

export const usePopUp = () => {
  const ctx = React.useContext(ModalContext);
  if (!ctx) throw new Error('invalid modal context');
  return ctx;
}

const PopUp = (props: {
  loading: boolean,
  opacity: UI.Animated.AnimatedInterpolation,
  style?: UI.ViewStyle,
  children?: React.ReactNode,
  onDimiss?: () => void,
}) =>
  <UI.Animated.View style={{ opacity: props.opacity }}>
    <UI.Breakable
      renderSmall={children =>
        <UI.View>
          <UI.TouchableWithoutFeedback disabled={props.loading}>
            <UI.View style={{ height: '100vh', padding: 40, backgroundColor: UI.Colors.white, alignItems: 'stretch', flex: 1, }}>
              <UI.View style={{ alignSelf: 'flex-start' }}>
                <UI.Icon disabled={props.loading} size="xs" name="times" onPress={() => props.onDimiss && props.onDimiss()} />
              </UI.View>
              <UI.View key={1} style={{ flex: 1 }}>
                {children}
              </UI.View>
            </UI.View>
          </UI.TouchableWithoutFeedback>
        </UI.View>
      }
      renderMedium={children =>
        <UI.View style={{ height: '100vh', justifyContent: 'center', alignItems: 'center' }}>
          <UI.TouchableWithoutFeedback disabled={props.loading}>
            <UI.View>
              <UI.View key={1} style={{ padding: 40, backgroundColor: UI.Colors.white, ...props.style }}>
                {children}
              </UI.View>
            </UI.View>
          </UI.TouchableWithoutFeedback>
        </UI.View>
      }
    >
      <UI.Breakable
        renderMedium={() =>
          <UI.View style={{ alignSelf: 'flex-start' }}>
            <UI.Icon disabled={props.loading} size="xs" name="times" onPress={() => props.onDimiss && props.onDimiss()} />
            <UI.Spacer />
          </UI.View>} />
      <UI.Spacer />
      {props.children}
    </UI.Breakable>
  </UI.Animated.View>

export const PopUpProvider = (props: {
  style?: UI.ViewStyle,
  children?: React.ReactNode,
  initialDismissable?: boolean,
}) => {
  const { loading } = UI.useLoading(PopUpProvider);

  const [current, setCurrent] = React.useState<React.ReactNode>(null);
  const { display, dismiss, requestDismissal } = useTopViewStack(PopUpProvider, {
    onDismissRequest: () => {
      if (!loading) {
        requestDismissal();
        setOpacity(0, () =>
          dismiss());
      }
    }
  });
  const [opacity, setOpacity] = useScalarAnimation(0);

  React.useEffect(() => {
    if (!current) {
      requestDismissal();
      setOpacity(0, () =>
        dismiss());
    } else {
      display(() =>
        <PopUp
          loading={loading}
          opacity={opacity}
          style={props.style}
          onDimiss={() => setCurrent(null)}>
          {current}
        </PopUp>);
      setOpacity(1);
    }
  }, [current]);

  React.useEffect(() => {
    if (current) {
      display(() =>
        <PopUp
          loading={loading}
          opacity={opacity}
          style={props.style}
          onDimiss={() => setCurrent(null)}>
          {current}
        </PopUp>);
    }
  }, [loading]);

  return (
    <ModalContext.Provider value={{ current, setCurrent }}>
      {props.children}
    </ModalContext.Provider>
  );
}
