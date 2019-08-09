import '../index.css';

export * from 'core-ui';

// injector

export * from './components/injector';

// root
export * from './components/root';

// web components
export * from './components/webnav';
export * from './components/webanchor';
export * from './components/webendreacheddetector';
export * from './components/webtable';
export * from './components/webcode';

// hooks
export * from './hooks/usetopviewstack';
export * from './hooks/usetoast';
export * from './hooks/usebodyscrolllocker';
export * from './hooks/useanimation';
export * from './hooks/useslideupanimation';
export * from './hooks/usescalaranimation';
export * from './hooks/usepopup';
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