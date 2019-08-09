import * as React from 'react';
import { WebCode, ScrollView, ViewStyle } from 'gatsby-theme-core-ui';

export const Terminal = (props: { lines: string[], style?: ViewStyle, secondary?: boolean }) => {
  const scrollViewRef = React.useRef<ScrollView | null>(null);
  React.useEffect(() => {
    if (scrollViewRef.current)
      scrollViewRef.current.scrollToEnd({ animated: false })
  }, [props.lines.length, scrollViewRef.current]);
  return (
    <ScrollView ref={r => scrollViewRef.current = r} style={props.style}>
      {props.lines.length > 0 && <WebCode secondary={props.secondary}>{props.lines.join('\n')}</WebCode>}
    </ScrollView>
  );
}
