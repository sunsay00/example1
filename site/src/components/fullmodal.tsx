import * as React from 'react';
import * as UI from 'gatsby-theme-core-ui';

export const FullModalContext = React.createContext({ visible: false });

export const FullModal = (props: {
  style?: UI.ViewStyle,
  visible: boolean,
  onDismiss?: () => void, children?: React.ReactNode
}) => {
  console.log('visible', props.visible);
  return (
    <UI.Modal animationType="fade" transparent visible={props.visible} onDismiss={() => props.onDismiss && props.onDismiss()}>
      <FullModalContext.Provider value={{ visible: props.visible }}>
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
                    <UI.Icon size="xs" name="times" onPress={() => props.onDismiss && props.onDismiss()} />
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
                <UI.Icon size="xs" name="times" onPress={() => props.onDismiss && props.onDismiss()} />
                <UI.Spacer />
              </UI.View>} />
          <UI.Spacer />
          {props.children}
        </UI.Breakable>
      </FullModalContext.Provider>
    </UI.Modal>
  );
}