import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { Animated, Dimensions, Easing, TouchableWithoutFeedback } from 'react-native';
import * as UI from 'core-ui';

const ModalPortal = (props: {
  children?: React.ReactNode
}) => {
  const [elem, setElem] = React.useState<HTMLElement>();

  React.useEffect(() => {
    const e = document.createElement('div');
    document.body.append(e);
    setElem(e);
    return () => {
      document.body.removeChild(e);
    };
  }, []);

  if (!elem) return null;
  return ReactDOM.createPortal(props.children, elem);
}

type ModalState = {
  animationSlide: Animated.CompositeAnimation | null,
  animationFade: Animated.CompositeAnimation | null,
  displayStyle: 'flex' | 'none',
  opacityFade: Animated.Value,
  slideTranslation: Animated.Value,
};

export class Modal extends React.Component<UI.ModalProps, ModalState> {

  static defaultProps = {
    animationType: 'none',
    transparent: false,
    visible: true,
    onShow: () => { },
    onRequestClose: () => { },
    onDismiss: () => { },
    ariaHideApp: true,
    appElement: null,
  } as const;

  constructor(props: Readonly<UI.ModalProps>) {
    super(props);

    this.state = {
      animationSlide: null,
      animationFade: null,
      displayStyle: props.visible ? 'flex' : 'none',
      opacityFade: new Animated.Value(0),
      slideTranslation: new Animated.Value(0),
    };
  }

  componentDidMount() {
    if (this.props.visible) this.handleShow(this.props);
  }

  componentWillReceiveProps(props: UI.ModalProps) {
    if (props.visible && !this.props.visible) this.handleShow(props);
    if (!props.visible && this.props.visible) this.handleClose(props);
  }

  private handleShow = (props: UI.ModalProps) => {
    if (props.animationType === 'slide') {
      this.animateSlideIn(() => { });
    } else if (props.animationType === 'fade') {
      this.animateFadeIn(() => { });
    }
  }

  dismiss = () => this.handleClose(this.props);

  private handleClose = (props: UI.ModalProps) => {
    if (props.animationType === 'slide') {
      this.animateSlideOut(() => props.onDismiss && props.onDismiss());
    } else if (props.animationType === 'fade') {
      this.animateFadeOut(() => props.onDismiss && props.onDismiss());
    } else {
      props.onDismiss && props.onDismiss();
    }
  }

  private animateFadeIn = (callback: Animated.EndCallback) => {
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

  private animateFadeOut = (callback: () => void) => {
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

  private animateSlideIn = (callback: Animated.EndCallback) => {
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

  private animateSlideOut = (callback: () => void) => {
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

  private getAnimationStyle = () => {
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
        <TouchableWithoutFeedback onPress={() => this.handleClose(this.props)}>
          <Animated.View
            aria-modal="true"
            style={{
              position: 'fixed',
              top: 0,
              right: 0,
              bottom: 0,
              left: 0,
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
