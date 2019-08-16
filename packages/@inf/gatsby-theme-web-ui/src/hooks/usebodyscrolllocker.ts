import * as React from 'react';

export const useBodyScrollLocker = (locked: boolean) => {
  const scrollLeftRef = React.useRef(0);
  const scrollTopRef = React.useRef(0);

  const lock = () => {
    scrollLeftRef.current = document.documentElement.scrollLeft || document.body.scrollLeft;
    scrollTopRef.current = document.documentElement.scrollTop || document.body.scrollTop;
    document.body.style.position = 'fixed';
    document.body.style.width = '100%';
    document.body.style.overflowY = 'scroll';
    document.body.style.top = `-${scrollTopRef.current}px`;
  };

  const unlock = () => {
    document.body.style.position = 'unset';
    document.body.style.width = 'unset';
    document.body.style.overflowY = 'unset';
    window.scrollTo(scrollLeftRef.current, scrollTopRef.current);
  };

  React.useEffect(() => {
    if (locked) lock();
    else unlock();
  }, [locked]);
}
