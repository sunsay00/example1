declare module '@fortawesome/react-fontawesome' {
  const FontAwesomeIcon: (props: {
    icon: string,
    style?: any,
  }) => JSX.Element;
}

declare module 'react-native-web/dist/modules/applyNativeMethods' {
  const applyNativeMethods: <P, S>(componentClass: React.ComponentClass<P, S>) => React.ComponentClass<P, S>;
  export default applyNativeMethods;
}