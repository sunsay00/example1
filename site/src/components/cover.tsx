import * as React from 'react';
import * as UI from '@inf/core-ui';

export const Cover = () =>
  <UI.Carousel infinite style={{ height: 500, marginHorizontal: -32 }}>
    <UI.ImageBackground
      source={{ uri: 'image1.jpg' }}
      style={{ flex: 1, width: '100%', height: '100%', zIndex: -1 }}>
      <UI.View style={{
        padding: 32, height: '100%',
        flex: 1, justifyContent: 'center', alignItems: 'center',
        backgroundColor: UI.rgba(UI.Colors.blue, .3)
      }}>
        <UI.Headline secondary>Business + Techology = Success</UI.Headline>
        <UI.Header3 secondary serifed weight="medium">Advocating for businesses since 2000</UI.Header3>
      </UI.View>
    </UI.ImageBackground>
  </UI.Carousel>