import * as React from 'react';
import { sg, Text } from 'gatsby-theme-core-ui';

const Headline = (sz: 1 | 2) => (props: { children?: React.ReactNode, serifed?: boolean, secondary?: boolean }) => {
  const fontSize = sg.fonts[props.serifed ? 'serif' : 'sansSerif'].size[sz == 1 ? 'super1' : 'super2'];
  const lineHeight = sg.fonts[props.serifed ? 'serif' : 'sansSerif'].lineHeight[sz == 1 ? 'super1' : 'super2'];
  const color = sg.rgba(props.secondary ? sg.colors.white : sg.colors.black, 1);
  return (
    <Text style={{
      textAlign: 'center',
      fontSize,
      lineHeight,
      marginVertical: 16,
    }}
      serifed={props.serifed}
      weight="bold"
      color={color}>
      {props.children}
    </Text>
  );
}

export const Headline1 = Headline(1);
export const Headline2 = Headline(2);