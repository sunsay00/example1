import * as React from 'react';
import { rgba, Colors, Carousel, View, Header3, ImageBackground } from 'gatsby-theme-core-ui';
import { Headline1 } from './headline';

export const Cover = () =>
  <Carousel infinite style={{ height: 500, marginHorizontal: -32 }}>
    <ImageBackground
      source={{ uri: 'image1.jpg' }}
      style={{ flex: 1, width: '100%', height: '100%', zIndex: -1 }}>
      <View style={{
        padding: 32, height: '100%',
        flex: 1, justifyContent: 'center', alignItems: 'center',
        backgroundColor: rgba(Colors.blue, .3)
      }}>
        <Headline1 secondary>Business + Techology = Success</Headline1>
        <Header3 secondary serifed weight="medium">Advocating for businesses since 2000</Header3>
      </View>
    </ImageBackground>
  </Carousel>