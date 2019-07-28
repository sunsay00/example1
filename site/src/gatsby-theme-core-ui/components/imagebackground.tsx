import * as React from 'react';
import BackgroundImage from 'gatsby-background-image';
import { ImageStyle, ImageResizeMode } from 'react-native';
import { StaticQuery, graphql } from 'gatsby';
import { convertImageStyle } from './image';

export const ImageBackground = (props: { source: { uri: string }, children?: React.ReactNode, style?: ImageStyle, resizeMode?: ImageResizeMode }) =>
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
      const BgImg = BackgroundImage as any;
      return (
        <BgImg style={convertImageStyle(props.style, props.resizeMode)} fluid={edge.node.childImageSharp.fluid}>{
          React.Children.map(props.children, child => React.isValidElement(child) && React.cloneElement(child,
            child.props
            //{ ...child.props, style: { height: undefined, ...child.props.style } }
          ) || null)
        }</BgImg>
      );
    }} />

