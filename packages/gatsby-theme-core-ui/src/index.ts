import '../index.css';

import { Image, ImageBackground, Modal } from 'core-ui';
export const Actual = { Image, ImageBackground, Modal };

export * from 'core-ui';

// core-ui overrides
export { Image } from './components/image';
export { ImageBackground } from './components/imagebackground';
export { Modal } from './components/modal';
export { Icon } from './components/icon';
export { AlertProvider, Alert } from './components/alert';
export { Slider } from './components/slider';

// web components
export { WebNavBar, WebNavAnchor, WebNavLayout, WebNavLink } from './components/webnav';
export { WebAnchor } from './components/webanchor';
export { WebEndReachedDetector } from './components/webendreacheddetector';
export { WebTable, WebTableBody, WebTableRow, WebTableData, WebTableHeader } from './components/webtable';
export { WebRoot } from './components/webroot';
export { WebCode, WebInlineCode } from './components/webcode';

// hooks
export * from './hooks/usetopviewstack';
export * from './hooks/usetoast';
export * from './hooks/usebodyscrolllocker';
export * from './hooks/useanimation';
export * from './hooks/useslideupanimation';
export * from './hooks/usescalaranimation';
export * from './hooks/usemodal';
export * from './hooks/useforms';

export * from './hooks/useaccount';

// icons
import { library } from '@fortawesome/fontawesome-svg-core';
import {
  faStroopwafel, faTimesCircle, faChevronLeft,
  faChevronRight, faUser, faLock, faEnvelope, faCheck
} from '@fortawesome/free-solid-svg-icons';

library.add(faStroopwafel);
library.add(faTimesCircle);
library.add(faChevronLeft);
library.add(faChevronRight);
library.add(faUser);
library.add(faLock);
library.add(faEnvelope);
library.add(faCheck);