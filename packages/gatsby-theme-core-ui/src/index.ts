import '../index.css';

// root
export * from './root';

// web components
export * from './components/nav';
export * from './components/anchor';
export * from './components/endreacheddetector';
export * from './components/table';
export * from './components/code';

// hooks
export * from './hooks/usebodyscrolllocker';
export * from './hooks/usepopup';

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