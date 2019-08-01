interface CognitoIdentityCredentials {
  needsRefresh(): boolean;
  clearCachedId(): void;
  get(next: (err: Error) => void): void;
  accessKeyId?: string;
  secretAccessKey?: string;
  sessionToken?: string;
}

declare module 'aws-sdk/dist/aws-sdk-react-native' {
  export const Config: any;
  export const CognitoIdentityCredentials: any;
  export const CognitoIdentityServiceProvider: any;
  const AWS: any;
  export default AWS;
}

declare module 'react-native-aws-cognito-js' {
  export const AuthenticationDetails: any;
  export const CognitoUserSession: any;
  export const CognitoUserPool: any;
  export const CognitoUser: any;
  export const CognitoUserAttribute: any;
}