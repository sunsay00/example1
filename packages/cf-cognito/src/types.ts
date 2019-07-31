export type Obj<T> = { [_: string]: T | undefined };

export type UserState = 'SignedOut' | 'SignedIn' | 'PendingConfirmation' | 'InvalidCredentials';

export type LocalStorage = {
  clear: () => Promise<void>;
  set: (key: string, value?: string) => Promise<void>
  get: (key: string) => Promise<string | undefined>
  setObject: (key: string, value: object) => Promise<void>;
  getObject: (key: string) => Promise<object | undefined>;
  getAllKeys: () => Promise<string[]>;
  remove: (key: string) => Promise<void>;
}

export enum UserPoolMode { Web, Mobile };

export type AccountCredentials = {
  accessKeyId: string,
  secretAccessKey: string,
  sessionToken: string,
};

export type AWSCognitoUserSession = {
  getAccessToken: () => {
    getJwtToken(): Promise<string>
  },
  getIdToken: () => {
    getJwtToken(): Promise<string>
  },
  getRefreshToken: () => {
    getToken(): Promise<string>
  },
}

export type AWSCognitoUserAttribute = {
  getName: () => string;
  getValue: () => any;
}

export type CompleteNewPasswordChallengeHandler = {
  onSuccess: (result: any) => void,
  onFailure: (err: Error) => void,
  mfaRequired: (codeDeliveryDetails: any) => void,
};

export type User = {
  getUsername: () => string,
  confirmRegistration(confirmationCode: string, b: boolean, next: (err: Error, data: any) => void): void,
  getSession(next: (err: Error, session: any) => void): void,
  updateAttributes: (attributeList: any[], next: (err: Error, result: string) => void) => void,
  getAttributeVerificationCode: (username: string, params: {
    onFailure: (err: any) => void,
    inputVerificationCode: (data: object) => void
  }) => void,
  verifyAttribute: (attribute: any, verificationCode: string, params: {
    onFailure: (err: any) => void,
    onSuccess: (data: string) => void
  }) => void,
  getUserAttributes: (next: (err: Error, result: AWSCognitoUserAttribute[]) => void) => void,
  authenticateUser: (details: AuthenticationDetails, params: {
    newPasswordRequired: (userAttributes: any, requiredAttributes: any) => void,
    customChallenge: (challengeParameters: any) => void,
    mfaRequired: (challengeName: any, challengeParameter: any) => void,
    onSuccess: (session: AWSCognitoUserSession) => void,
    onFailure: (err: any) => void
  }) => void,
  signOut: () => void,
  changePassword: (previousPassword: string, proposedPassword: string, next: (err: Error, result: any) => void) => void,
  completeNewPasswordChallenge: (newPassword: string, requiredAttributeData: Obj<string>, handler: CompleteNewPasswordChallengeHandler) => void,
  globalSignOut: (next: {
    onSuccess: (msg: string) => void,
    onFailure: (err: Error) => void
  }) => void,
  forgotPassword: (next: {
    onSuccess: (result: any) => void,
    onFailure: (err: Error) => void,
    inputVerificationCode: (result: any) => void
  }) => void,
  confirmPassword: (verificationCode: string, password: string, next: {
    onSuccess: () => void,
    onFailure: (err: Error) => void
  }) => void,
  refreshSession: (refreshToken: any, next: (err: Error, session: any) => void) => void,
}

export type UserPool = {
  signUp: (username: string, password: string, attributeList: any[], a: any, fn: (err: any, result: any) => void) => void,
  getCurrentUser: () => User,
}

export type Storage = {
  getItem(key: string, callback?: (error?: Error, result?: string) => void): Promise<string | null>;
  setItem(key: string, value: string, callback?: (error?: Error) => void): Promise<void>;
  removeItem(key: string, callback?: (error?: Error) => void): Promise<void>;
  clear(callback?: (error?: Error) => void): Promise<void>;
  getAllKeys(callback?: (error?: Error, keys?: string[]) => void): Promise<string[]>;
}
