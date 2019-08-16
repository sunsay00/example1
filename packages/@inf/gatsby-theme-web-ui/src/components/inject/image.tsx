import * as React from 'react';
import { Image as RNImage } from 'react-native';
import * as UI from '@inf/core-ui';
import { StaticQuery, graphql } from 'gatsby';
import GTImage from 'gatsby-image';

export const convertImageStyle = (style?: UI.ImageStyle, resizeMode?: UI.ImageResizeMode) => style && ({
  marginLeft: style.marginHorizontal || style.marginLeft,
  marginRight: style.marginHorizontal || style.marginRight,
  marginTop: style.marginVertical || style.marginTop,
  marginBottom: style.marginVertical || style.marginBottom,
  paddingLeft: style.paddingHorizontal || style.paddingLeft,
  paddingRight: style.paddingHorizontal || style.paddingRight,
  paddingTop: style.paddingVertical || style.paddingTop,
  paddingBottom: style.paddingVertical || style.paddingBottom,
  backgroundRepeat: resizeMode == 'repeat' ? 'repeat' : undefined,
  backgroundSize: resizeMode || 'cover',
  ...style,
});

export const Image = (props: UI.ImageProps) => {
  if (!props.source.uri) {
    return null;
  } else if (props.source.uri.startsWith('http')) {
    return <RNImage source={props.source} style={props.style} resizeMode={props.resizeMode} />;
  } else {
    return (
      <StaticQuery
        query={graphql`query Image {
  allFile(filter: {extension: {in: ["png", "jpg"]}}) {
    edges {
      node {
        name
        relativePath
        childImageSharp {
          fluid(maxWidth: 600) {
            ...GatsbyImageSharpFluid
          }
        }
      }
    }
  }
}`}
        render={data => {
          const edge = data.allFile.edges.find((e: any) => e.node.relativePath == props.source.uri);
          if (!edge) return null;
          return (
            <GTImage style={convertImageStyle(props.style, props.resizeMode)} fluid={edge.node.childImageSharp.fluid} />
          );
        }} />
    )
  }
}
