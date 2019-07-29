import * as React from 'react';
import * as UI from 'gatsby-theme-core-ui';

export const FullModal = (props: {
  style?: UI.ViewStyle,
  visible: boolean,
  onDismiss?: () => void, children?: React.ReactNode
}) => {
  return (
    <UI.Modal animationType="fade" transparent visible={props.visible} onDismiss={() => props.onDismiss && props.onDismiss()}>
      <UI.View style={{ flex: 1, justifyContent: 'center', alignItems: 'center', backgroundColor: UI.rgba('#000000', .65) }}>
        <UI.TouchableWithoutFeedback>
          <UI.View style={{
            padding: 40,
            backgroundColor: UI.Colors.white,
            ...props.style
          }}>
            <UI.View style={{ alignSelf: 'flex-start' }}>
              <UI.Icon size="xs" name="times" onPress={() => props.onDismiss && props.onDismiss()} />
            </UI.View>
            <UI.Spacer />
            {props.children}
          </UI.View>
        </UI.TouchableWithoutFeedback>
      </UI.View>
    </UI.Modal >
  );
}