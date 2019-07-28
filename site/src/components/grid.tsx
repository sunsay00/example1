import * as React from 'react';
import { UI } from 'gatsby-theme-core-ui';
import { Breakable } from './breakable';

export const Grid = (props: { children?: React.ReactNode, stride: number }) => {
  const children = React.Children.toArray(props.children);
  const count = children.length;
  const mod = count % props.stride;
  const rows = (count - mod) / props.stride;
  return (
    <UI.View style={{ flex: 1, marginVertical: 16, alignItems: 'center' }}>
      <Breakable
        renderSmall={() =>
          <>{Array(count).fill(0).map((_, i) =>
            <UI.View key={i} style={{ flex: 1, marginVertical: 16, marginHorizontal: 16, maxWidth: 480 }}>{children[i]}</UI.View>)}
          </>}
        renderMedium={() =>
          <>{Array(rows + (mod > 0 ? 1 : 0)).fill(0).map((_, j) =>
            <UI.View key={j} style={{ justifyContent: 'space-evenly', flexDirection: 'row', flex: 1, marginVertical: 16 }}>
              {Array(props.stride).fill(0).map((_, i) =>
                <UI.View key={i} style={{ flex: 1, marginHorizontal: 16 }}>
                  {rows != j || (rows == j && i < mod) ? children[j * props.stride + i] : null}
                </UI.View>)}
            </UI.View>)}
          </>}
      />
    </UI.View>
  );
}
