import { UI as CoreUI, sg as coresg } from 'core-ui';
import { Image } from './src/components/image';
import { ImageBackground } from './src/components/imagebackground';

export const UI = {
  ...CoreUI,
  Image,
  ImageBackground,
  Actual: {
    Image: CoreUI.Image,
    ImageBackground: CoreUI.ImageBackground
  }
};

export const sg = coresg;