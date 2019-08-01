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

export type CognitoUserSession = {
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

export type CognitoUserAttribute = {
  getName: () => string;
  getValue: () => any;
}

export type CompleteNewPasswordChallengeHandler = {
  onSuccess: (result: any) => void,
  onFailure: (err: Error) => void,
  mfaRequired: (codeDeliveryDetails: any) => void,
};

export type UserPool = {
  signUp: (username: string, password: string, attributeList: any[], a: any, fn: (err: any, result: any) => void) => void,
  getCurrentUser: () => CognitoUser,
}

export type Storage = {
  getItem(key: string, callback?: (error?: Error, result?: string) => void): Promise<string | null>;
  setItem(key: string, value: string, callback?: (error?: Error) => void): Promise<void>;
  removeItem(key: string, callback?: (error?: Error) => void): Promise<void>;
  clear(callback?: (error?: Error) => void): Promise<void>;
  getAllKeys(callback?: (error?: Error, keys?: string[]) => void): Promise<string[]>;
}

export type CognitoUser = {
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
  getUserAttributes: (next: (err: Error, result: CognitoUserAttribute[]) => void) => void,
  authenticateUser: (details: AuthenticationDetails, params: {
    newPasswordRequired: (userAttributes: any, requiredAttributes: any) => void,
    customChallenge: (challengeParameters: any) => void,
    mfaRequired: (challengeName: any, challengeParameter: any) => void,
    onSuccess: (session: CognitoUserSession) => void,
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

export type CognitoIdentityServiceProvider = {
  resendConfirmationCode(params: { ClientId: string, Username: string }): { promise: () => Promise<void> }
};
export type AuthenticationDetails = {};

export type CognitoClient = {
  refreshCredentials(accessKeyId: string, secretAccessKey: string, sessionToken: string): void;
  setCognitoIdentityPoolDetails(region: string, logins?: Obj<string>): CognitoIdentityCredentials;
  createCognitoUserPool(mode: UserPoolMode): UserPool;
  createCognitoUser(region: string, mode: UserPoolMode, Username: string): CognitoUser;
  cognitoIdentityId(): string;
  currentUser(region: string, mode: UserPoolMode): CognitoUser;
  accessKeyId(): string | undefined;
  secretAccessKey(): string | undefined;
  sessionToken(): string | undefined;
  clearCachedId(): void;
  createCognitoUserAttribute(Name: string, Value: string): CognitoUserAttribute;
  createCognitoServiceProvider(): CognitoIdentityServiceProvider;
  createAuthenticationDetails(Name: string, Password: String): AuthenticationDetails;
};