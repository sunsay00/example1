import * as React from 'react';
import * as UI from 'gatsby-theme-core-ui';

type ContextValue = {
  current: JSX.Element | null,
  setCurrent: (c: JSX.Element | null) => void,
};

const ModalContext = React.createContext<ContextValue>({ current: null, setCurrent: _ => console.warn('invalid modal context') });

export const useModal = () => React.useContext(ModalContext);

export const ModalProvider = (props: {
  style?: UI.ViewStyle,
  children?: React.ReactNode,
}) => {
  const [current, setCurrent] = React.useState<JSX.Element | null>(null);
  const stack = UI.useTopViewStack();

  React.useEffect(() => stack.register(ModalProvider, {
    animationType: 'fade',
    onDismiss: () => setCurrent(null)
  }), []);

  React.useEffect(() => {
    if (current) {
      stack.push(ModalProvider,
        <UI.Breakable
          renderSmall={children =>
            <UI.View style={{ flex: 1 }}>
              <UI.TouchableWithoutFeedback>
                <UI.View style={{
                  padding: 40,
                  backgroundColor: UI.Colors.white,
                  alignItems: 'stretch',
                  flex: 1,
                }}>
                  <UI.View style={{ alignSelf: 'flex-start' }}>
                    <UI.Icon size="xs" name="times" onPress={() => setCurrent(null)} />
                  </UI.View>
                  <UI.View key={1} style={{ flex: 1 }}>{children}</UI.View>
                </UI.View>
              </UI.TouchableWithoutFeedback>
            </UI.View>
          }
          renderMedium={children =>
            <UI.View style={{ flex: 1, justifyContent: 'center', alignItems: 'center' }}>
              <UI.TouchableWithoutFeedback>
                <UI.View style={{
                  padding: 40,
                  backgroundColor: UI.Colors.white,
                  ...props.style
                }}>
                  <UI.View key={1} style={{ flex: 1 }}>{children}</UI.View>
                </UI.View>
              </UI.TouchableWithoutFeedback>
            </UI.View>
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
      );
    } else {
      stack.pop(ModalProvider);
    }
  }, [!!current]);

  return (
    <ModalContext.Provider value={{ current, setCurrent }}>
      {props.children}
    </ModalContext.Provider>
  );
}
