import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { Animated, Dimensions, Easing, TouchableWithoutFeedback } from '..';

type ModalPortalProps = {
  children: React.ReactNode
};

type ModalPortalState = {
  el: HTMLElement | null,
  target: HTMLElement | null
}

class ModalPortal extends React.Component<ModalPortalProps, ModalPortalState> {
  constructor(props: { children: React.ReactNode }) {
    super(props);
    this.state = {
      el: null,
      target: null
    };
  }
  componentDidMount() {
    this.setState({ el: document.createElement('div'), target: document.body },
      () => {
        if (this.state.target && this.state.el)
          this.state.target.appendChild(this.state.el);
      }
    );
  }
  componentWillUnmount() {
    if (this.state.target && this.state.el)
      this.state.target.removeChild(this.state.el);
  }
  render() {
    const { children } = this.props;
    if (this.state.el)
      return ReactDOM.createPortal(children, this.state.el);
    return null;
  }
}

export type ModalProps = {
  animationType?: 'none' | 'slide' | 'fade',
  transparent?: boolean,
  visible?: boolean,
  onRequestClose?: () => void,
  onShow?: () => void,
  onDismiss?: () => void,
  children?: React.ReactNode,
}

type ModalState = {
  animationSlide: Animated.CompositeAnimation | null,
  animationFade: Animated.CompositeAnimation | null,
  displayStyle: 'flex' | 'none',
  opacityFade: Animated.Value,
  slideTranslation: Animated.Value,
};

export class Modal extends React.Component<ModalProps, ModalState> {

  static defaultProps = {
    animationType: 'none',
    transparent: false,
    visible: true,
    onShow: () => { },
    onRequestClose: () => { },
    onDismiss: () => { },
    ariaHideApp: true,
    appElement: null,
  };

  constructor(props: Readonly<ModalProps>) {
    super(props);

    this.state = {
      animationSlide: null,
      animationFade: null,
      displayStyle: props.visible ? 'flex' : 'none',
      opacityFade: new Animated.Value(0),
      slideTranslation: new Animated.Value(0),
    };
    console.log('M-CTOR');
  }

  componentDidMount() {
    console.log('M-CDM', this.props.visible);
    if (this.props.visible) this.handleShow();
  }

  componentWillReceiveProps({ visible }: ModalProps) {
    console.log('M-WRP', visible, this.props.visible);
    if (visible && !this.props.visible) this.handleShow();
    if (!visible && this.props.visible) this.handleClose();
  }

  scrollLeft = 0;
  scrollTop = 0;

  handleShow() {
    const { animationType, onShow = () => { } } = this.props;
    if (animationType === 'slide') {
      this.animateSlideIn(onShow);
    } else if (animationType === 'fade') {
      this.animateFadeIn(onShow);
    } else {
      onShow();
    }

    // lock body scrolling
    this.scrollLeft = document.documentElement.scrollLeft || document.body.scrollLeft;
    this.scrollTop = document.documentElement.scrollTop || document.body.scrollTop;
    document.body.style.position = 'fixed';
    document.body.style.width = '100%';
    document.body.style.overflowY = 'scroll';
    document.body.style.top = `-${this.scrollTop}px`;
  }

  handleClose = () => {
    // unlock body scolling
    document.body.style.position = 'unset';
    document.body.style.width = 'unset';
    document.body.style.overflowY = 'unset';
    window.scrollTo(this.scrollLeft, this.scrollTop);

    const { animationType, onDismiss = () => { } } = this.props;
    if (animationType === 'slide') {
      this.animateSlideOut(onDismiss);
    } else if (animationType === 'fade') {
      this.animateFadeOut(onDismiss);
    } else {
      onDismiss();
    }
  }

  animateFadeIn = (callback: Animated.EndCallback) => {
    if (this.state.animationFade)
      this.state.animationFade.stop();

    const animationFade = Animated.timing(this.state.opacityFade, {
      toValue: 1,
      duration: 300,
    });

    this.setState({ animationFade, }, () => {
      requestAnimationFrame(() => {
        this.setState({ displayStyle: 'flex' }, () =>
          this.state.animationFade && this.state.animationFade.start(callback)
        );
      });
    }
    );
  };

  animateFadeOut = (callback: () => void) => {
    if (this.state.animationFade)
      this.state.animationFade.stop();

    const animationFade = Animated.timing(this.state.opacityFade, {
      toValue: 0,
      duration: 300,
    });

    this.setState({ animationFade, }, () => {
      requestAnimationFrame(() => {
        this.state.animationFade && this.state.animationFade.start(() => {
          this.setState({ displayStyle: 'none' },
            callback
          );
        });
      });
    }
    );
  };

  animateSlideIn = (callback: Animated.EndCallback) => {
    if (this.state.animationSlide) {
      this.state.animationSlide.stop();
    }

    const animationSlide = Animated.timing(this.state.slideTranslation, {
      toValue: 1,
      easing: Easing.out(Easing.poly(4)),
      duration: 300,
    });

    this.setState({ animationSlide }, () => {
      requestAnimationFrame(() => {
        this.setState({ displayStyle: 'flex' }, () =>
          this.state.animationSlide && this.state.animationSlide.start(callback)
        );
      });
    });
  };

  animateSlideOut = (callback: () => void) => {
    if (this.state.animationSlide)
      this.state.animationSlide.stop();

    const animationSlide = Animated.timing(this.state.slideTranslation, {
      toValue: 0,
      easing: Easing.in(Easing.poly(4)),
      duration: 300,
    });

    this.setState({ animationSlide, }, () => {
      requestAnimationFrame(() => {
        this.state.animationSlide && this.state.animationSlide.start(() => {
          this.setState({ displayStyle: 'none' },
            callback
          );
        });
      });
    });
  };

  getAnimationStyle = () => {
    const { visible, animationType } = this.props;
    const display = this.state.displayStyle;
    if (animationType === 'slide') {
      return {
        display,
        transform: [{
          translateY: this.state.slideTranslation.interpolate({
            inputRange: [0, 1],
            outputRange: [Dimensions.get('window').height, 0],
            extrapolate: 'clamp',
          }),
        }],
      };
    } else if (animationType === 'fade') {
      return { opacity: this.state.opacityFade, display };
    } else {
      return { display: visible ? 'flex' : 'none' };
    }
  }

  render() {
    const { transparent, children } = this.props;
    return (
      <ModalPortal>
        <TouchableWithoutFeedback onPress={() => this.handleClose()}>
          <Animated.View
            aria-modal="true"
            style={{
              position: 'fixed',
              top: 0,
              right: 0,
              bottom: 0,
              left: 0,
              zIndex: 9999,
              backgroundColor: transparent ? 'transparent' : '#ffffff',
              ...this.getAnimationStyle()
            }}
          >
            {children}
          </Animated.View>
        </TouchableWithoutFeedback>
      </ModalPortal>
    );
  }
}
