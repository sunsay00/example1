import * as React from 'react';
import { ImageStyle, ImageResizeMode } from 'react-native';
import { StaticQuery, graphql } from 'gatsby';
import GTImage from 'gatsby-image';
import { Actual } from 'gatsby-theme-core-ui';

export const convertImageStyle = (style?: ImageStyle, resizeMode?: ImageResizeMode) => style && ({
  marginLeft: style.marginHorizontal || style.marginLeft,
  marginRight: style.marginHorizontal || style.marginRight,
  marginTop: style.marginVertical || style.marginTop,
  marginBottom: style.paddingVertical || style.marginBottom,
  paddingLeft: style.paddingHorizontal || style.paddingLeft,
  paddingRight: style.paddingHorizontal || style.paddingRight,
  paddingTop: style.paddingVertical || style.paddingTop,
  paddingBottom: style.paddingVertical || style.paddingBottom,
  backgroundRepeat: resizeMode == 'repeat' ? 'repeat' : undefined,
  backgroundSize: resizeMode || 'cover',
  ...style,
});

export const Image = (props: { source: { uri: string }, style?: ImageStyle, resizeMode?: ImageResizeMode }) => {
  if (props.source.uri.startsWith('http')) {
    return <Actual.Image source={props.source} style={props.style} resizeMode={props.resizeMode} />;
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
