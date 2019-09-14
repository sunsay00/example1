import * as React from 'react';
import * as UI from '@inf/core-ui';

export default () => {
  return (
    <>
      <UI.Button onPress={() => UI.Alert.alert('hello')}>
        Testing
      </UI.Button>
    </>
  );
}
