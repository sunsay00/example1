import * as React from 'react';
import { Colors, rgba, Modal, View } from 'gatsby-theme-core-ui';

export const FullModal = (props: { visible: boolean, onDismiss?: () => void, children?: React.ReactNode }) => {
  return (
    <Modal animationType="fade" transparent visible={props.visible} onDismiss={() => props.onDismiss && props.onDismiss()}>
      <View style={{ flex: 1, justifyContent: 'center', alignItems: 'center', backgroundColor: rgba('#000000', .65) }}>
        <View style={{ backgroundColor: Colors.white, padding: 32 }}>
          {props.children}
        </View>
      </View>
    </Modal >
  );
}