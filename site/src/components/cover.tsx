import * as React from 'react';
import { UI, sg } from 'gatsby-theme-core-ui';
import { Headline1 } from './headline';

export const Cover = () =>
  <UI.Carousel infinite style={{ height: 500, marginHorizontal: -32 }}>
    <UI.ImageBackground
      source={{ uri: 'image1.jpg' }}
      style={{ flex: 1, width: '100%', height: '100%', zIndex: -1 }}>
      <UI.View style={{
        padding: 32, height: '100%',
        flex: 1, justifyContent: 'center', alignItems: 'center',
        backgroundColor: sg.rgba(sg.colors.blue, .3)
      }}>
        <Headline1 secondary>Business + Techology = Success</Headline1>
        <UI.Header3 secondary serifed weight="medium">Advocating for businesses since 2000</UI.Header3>
      </UI.View>
    </UI.ImageBackground>
  </UI.Carousel>