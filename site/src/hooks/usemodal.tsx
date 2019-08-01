import * as React from 'react';
import * as UI from 'gatsby-theme-core-ui';

type ContextValue = {
  current: JSX.Element | undefined,
  setCurrent: (c: JSX.Element | undefined) => void,
};

const ModalContext = React.createContext<ContextValue>({ current: undefined, setCurrent: _ => { } });

export const useModal = () => React.useContext(ModalContext);

export const ModalProvider = (props: {
  style?: UI.ViewStyle,
  children?: React.ReactNode,
  renderModal?: (elem: JSX.Element) => JSX.Element,
}) => {
  const wrapper = props.renderModal || ((elem: JSX.Element) => elem);
  const [current, setCurrent] = React.useState<JSX.Element | undefined>(undefined);
  return (
    <>
      <ModalContext.Provider value={{ current, setCurrent }}>
        {props.children}
      </ModalContext.Provider>
      <UI.Modal animationType="fade" transparent visible={!!current} onDismiss={() => setCurrent(undefined)}>
        {wrapper(
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
                      <UI.Icon size="xs" name="times" onPress={() => setCurrent(undefined)} />
                    </UI.View>
                    <UI.View key={1} style={{ flex: 1 }}>{children}</UI.View>
                  </UI.View>
                </UI.TouchableWithoutFeedback>
              </UI.View>
            }
            renderMedium={children =>
              <UI.View style={{ flex: 1, justifyContent: 'center', alignItems: 'center', backgroundColor: UI.rgba('#000000', .65) }}>
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
                  <UI.Icon size="xs" name="times" onPress={() => setCurrent(undefined)} />
                  <UI.Spacer />
                </UI.View>} />
            <UI.Spacer />
            {current}
          </UI.Breakable>)}
      </UI.Modal>
    </>
  );
}
