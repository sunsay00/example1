import * as React from 'react';
import * as UI from 'core-ui';

export const FixedView = (props: { style?: UI.ViewStyle, children?: React.ReactNode }) => {
  const style = (props.style || {}) as React.CSSProperties;
  return (
    <div style={{
      ...style,
      position: 'fixed',
      top: 0,
      left: 0,
      width: '100%',
      height: '100%',
      display: 'flex',
      justifyContent: 'center',
      alignItems: 'center',
    }}>
      <div style={{ display: 'flex' }}>
        {props.children}
      </div>
    </div>
  );
}