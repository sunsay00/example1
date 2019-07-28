import * as React from 'react';
import { Dimensions } from 'react-native';

const calcMode = () => {
  const width = Dimensions.get('window').width;
  if (width < 768) return 0;
  else if (width < 992) return 1;
  else return 2;
};

export const Breakable_ = (props: {
  renderSmall?: () => JSX.Element, // 1 - 785
  renderMedium?: () => JSX.Element, // 786 - 991
  renderLarge?: () => JSX.Element, // 992 -
  children?: (result: React.ReactNode) => JSX.Element
}): JSX.Element => {
  const [mode, setMode] = React.useState(calcMode);
  React.useEffect(() => {
    const fn = () => setMode(calcMode());
    Dimensions.addEventListener('change', fn);
    return () => Dimensions.removeEventListener('change', fn);
  }, []);
  const ret: JSX.Element =
    <>
      {mode == 0 && props.renderSmall && props.renderSmall() ||
        mode == 1 && props.renderMedium && props.renderMedium() ||
        props.renderLarge && props.renderLarge() ||
        props.renderMedium && props.renderMedium() ||
        props.renderSmall && props.renderSmall() || null}
    </>;
  return props.children && props.children(ret) || ret;
}

export const Breakable = (props: {
  renderSmall?: (children: React.ReactNode) => JSX.Element, // 1 - 785
  renderMedium?: (children: React.ReactNode) => JSX.Element, // 786 - 991
  renderLarge?: (children: React.ReactNode) => JSX.Element, // 992 -
  children?: React.ReactNode
}): JSX.Element => {
  const [mode, setMode] = React.useState(calcMode);
  React.useEffect(() => {
    const fn = () => setMode(calcMode());
    Dimensions.addEventListener('change', fn);
    return () => Dimensions.removeEventListener('change', fn);
  }, []);
  const children = props.children || null;
  return (
    <>
      {mode == 0 && props.renderSmall && props.renderSmall(children) ||
        mode == 1 && props.renderMedium && props.renderMedium(children) ||
        props.renderLarge && props.renderLarge(children) ||
        props.renderMedium && props.renderMedium(children) ||
        props.renderSmall && props.renderSmall(children) || null}
    </>
  );
}