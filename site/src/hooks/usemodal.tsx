import * as React from 'react';
import * as UI from 'gatsby-theme-core-ui';

type ContextValue = {
  current: React.ReactNode,
  setCurrent: (c: React.ReactNode) => void,
};

const ModalContext = React.createContext<ContextValue>({ current: null, setCurrent: _ => console.warn('invalid modal context') });

export const useModal = () => React.useContext(ModalContext);

export const ModalProvider = (props: {
  style?: UI.ViewStyle,
  children?: React.ReactNode,
}) => {
  const [current, setCurrent] = React.useState<React.ReactNode>(null);
  const { display, dismiss, requestDismissal } = UI.useTopViewStack(ModalProvider, {
    onDismissRequest: () => {
      requestDismissal();
      setOpacity(0, () =>
        dismiss());
    }
  });
  const [opacity, setOpacity] = UI.useScalarAnimation(0);

  React.useEffect(() => {
    if (!current) {
      requestDismissal();
      setOpacity(0, () =>
        dismiss());
    } else {
      display(() =>
        <UI.Animated.View style={{ opacity }}>
          <UI.Breakable
            renderSmall={children =>
              <UI.TouchableWithoutFeedback>
                <UI.View style={{ height: '100vh', padding: 40, backgroundColor: UI.Colors.white, alignItems: 'stretch', flex: 1, }}>
                  <UI.View style={{ alignSelf: 'flex-start' }}>
                    <UI.Icon size="xs" name="times" onPress={() => setCurrent(null)} />
                  </UI.View>
                  <UI.View key={1} style={{ flex: 1 }}>
                    {children}
                  </UI.View>
                </UI.View>
              </UI.TouchableWithoutFeedback>
            }
            renderMedium={children =>
              <UI.TouchableWithoutFeedback>
                <UI.View style={{ height: '100vh', justifyContent: 'center', alignItems: 'center' }}>
                  <UI.View key={1} style={{ padding: 40, backgroundColor: UI.Colors.white, ...props.style }}>
                    {children}
                  </UI.View>
                </UI.View>
              </UI.TouchableWithoutFeedback>
            }
          >
            <UI.Breakable
              renderMedium={() =>
                <UI.View style={{ alignSelf: 'flex-start' }}>
                  <UI.Icon size="xs" name="times" onPress={() => setCurrent(null)} />
                  <UI.Spacer />
                </UI.View>} />
            <UI.Spacer />
            {current}
          </UI.Breakable>
        </UI.Animated.View>
      );
      setOpacity(1);
    }
  }, [current]);

  return (
    <ModalContext.Provider value={{ current, setCurrent }}>
      {props.children}
    </ModalContext.Provider>
  );
}
