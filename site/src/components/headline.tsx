import * as React from 'react';
import * as UI from 'gatsby-theme-core-ui';

const Headline = (sz: 1 | 2) => (props: { children?: React.ReactNode, serifed?: boolean, secondary?: boolean }) => {
  const fontSize = UI.Fonts[props.serifed ? 'serif' : 'sansSerif'].size[sz == 1 ? 'super1' : 'super2'];
  const lineHeight = UI.Fonts[props.serifed ? 'serif' : 'sansSerif'].lineHeight[sz == 1 ? 'super1' : 'super2'];
  const color = UI.rgba(props.secondary ? UI.Colors.white : UI.Colors.black, 1);
  return (
    <UI.Text style={{
      textAlign: 'center',
      fontSize,
      lineHeight,
      marginVertical: 16,
    }}
      serifed={props.serifed}
      weight="bold"
      color={color}>
      {props.children}
    </UI.Text>
  );
}

export const Headline1 = Headline(1);
export const Headline2 = Headline(2);