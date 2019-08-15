declare module 'twilio-common' {
  class AccessManager {
    constructor(token: string);
    on(type: 'tokenUpdated' | 'tokenExpired', callback: (param?: any) => void): void;
    updateToken(newToken: string): void;
  }
  export const AccessManager;
}