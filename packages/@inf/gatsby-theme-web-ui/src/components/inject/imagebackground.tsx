import * as React from 'react';
import GTBackgroundImage from 'gatsby-background-image';
import * as UI from '@inf/core-ui';
import { StaticQuery, graphql } from 'gatsby';

export const convertImageBackgroundStyle = (imageStyle?: UI.ImageStyle, style?: UI.ViewStyle, resizeMode?: UI.ImageResizeMode) => ({
  ...style,
  ...imageStyle,
  marginLeft: imageStyle && (imageStyle.marginHorizontal || imageStyle.marginLeft) || (style && (style.marginHorizontal || style.marginLeft)),
  marginRight: imageStyle && (imageStyle.marginHorizontal || imageStyle.marginRight) || (style && (style.marginHorizontal || style.marginRight)),
  marginTop: imageStyle && (imageStyle.marginVertical || imageStyle.marginTop) || (style && (style.marginVertical || style.marginTop)),
  marginBottom: imageStyle && (imageStyle.marginVertical || imageStyle.marginBottom) || (style && (style.marginVertical || style.marginBottom)),
  paddingLeft: imageStyle && (imageStyle.paddingHorizontal || imageStyle.paddingLeft) || (style && (style.paddingHorizontal || style.paddingLeft)),
  paddingRight: imageStyle && (imageStyle.paddingHorizontal || imageStyle.paddingRight) || (style && (style.paddingHorizontal || style.paddingRight)),
  paddingTop: imageStyle && (imageStyle.paddingVertical || imageStyle.paddingTop) || (style && (style.paddingVertical || style.paddingTop)),
  paddingBottom: imageStyle && (imageStyle.paddingVertical || imageStyle.paddingBottom) || (style && (style.paddingVertical || style.paddingBottom)),
  backgroundRepeat: resizeMode == 'repeat' ? 'repeat' : undefined,
  backgroundSize: resizeMode || 'cover',
});

export const ImageBackground = (props: UI.ImageBackgroundProps) =>
  <StaticQuery
    query={graphql`query ImageBackground {
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
      const BgImg = GTBackgroundImage as any;
      return (
        <BgImg style={convertImageBackgroundStyle(props.imageStyle, props.style, props.resizeMode)} fluid={edge.node.childImageSharp.fluid}>{
          React.Children.map(props.children, child => React.isValidElement(child) && React.cloneElement(child,
            child.props
            //{ ...child.props, style: { height: undefined, ...child.props.style } }
          ) || null)
        }</BgImg>
      );
    }} />

