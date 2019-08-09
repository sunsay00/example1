import * as React from 'react';
import { useState, useContext } from 'react';
import { AsyncStorage } from 'react-native';
import { Account } from '../account';
import { UserPoolMode } from '../types';
import * as UI from 'core-ui';

export enum LogInResult { Success, ChangePassword, UserNotFound, NotAuthorized, UserNotConfirmed, Unknown };
export enum SignUpResult { Success, UsernameExists, Unknown };
export enum ConfirmResult { Success, CodeMismatch, Unknown };

enum AccountMode { LoggedIn, LoggedOut };

export type AccountUser = {
  username: string,
  sub: string,
  locale: string,
  email: string,
  role: string,
  groups: string[],
  tokens: {
    accessToken: string,
    idToken: string,
    awsAccessKeyId: string,
    awsSecretAccessKey: string,
    refreshToken: string,
  }
};

type ContextValue = {
  account: Account
  setMode: (mode: AccountMode) => void,
  user?: AccountUser,
};

const _account = new Account(UserPoolMode.Web, AsyncStorage);

const AccountContext = React.createContext<ContextValue | undefined>(undefined);

export const useAccount = () => {
  const toast = UI.useToast();
  const ctx = useContext(AccountContext);
  if (!ctx) throw new Error('invalid account context');

  const { account, setMode, user } = ctx;

  const { setLoading } = UI.useLoading(useAccount);

  const resendConfirmationCode = async () => {
    try {
      setLoading(true);
      await account.resendConfirmationCode();
      toast.success('Confirmation code has been resent');
    } catch (err) {
      toast.error(err);
    } finally {
      setLoading(false);
    }
  }

  const logOut = async () => {
    try {
      setLoading(true);
      await account.signOut();
      setMode(AccountMode.LoggedOut);
    } finally {
      setLoading(false);
    }
  }

  const logIn = async (emailOrUsername: string, password: string): Promise<LogInResult> => {
    try {
      setLoading(true);
      const result = await account.signIn(emailOrUsername, password);
      if (result == 'changepassword')
        return LogInResult.ChangePassword;
      setMode(AccountMode.LoggedIn);
      return LogInResult.Success;
    } catch (err) {
      if (err.code == 'UserNotFoundException') {
        return LogInResult.UserNotFound;
      } else if (err.code == 'NotAuthorizedException') {
        return LogInResult.NotAuthorized;
      } else if (err.code == 'UserNotConfirmedException') {
        return LogInResult.UserNotConfirmed;
      } else {
        toast.error(`${err.message} (${err.code})`);
        return LogInResult.Unknown;
      }
    } finally {
      setLoading(false);
    }
  };

  const signUp = async (username: string, email: string, password: string, locale: string, role: string): Promise<SignUpResult> => {
    try {
      setLoading(true);
      const verifiedUsername = await account.signUp(username, email, password, locale, role);
      if (verifiedUsername != username) throw new Error('username mismatch');
      return SignUpResult.Success;
    } catch (err) {
      if (err.code == 'UsernameExistsException') {
        return SignUpResult.UsernameExists
      } else {
        toast.error(`${err.message} (${err.code})`);
        return SignUpResult.Unknown;
      }
    } finally {
      setLoading(false);
    }
  }

  const sendRecoveryEmail = async (emailOrUsername: string) => {
    try {
      setLoading(true);
      await account.forgotPassword(emailOrUsername);
    } catch (err) {
      toast.error(err);
    } finally {
      setLoading(false);
    }
  }

  const resetPassword = async (emailOrUsername: string, code: string, newPassword: string) => {
    try {
      setLoading(true);
      await account.confirmForgotPassword(emailOrUsername, code, newPassword);
      toast.info('Password has successfully been reset');
    } catch (err) {
      toast.error(err);
    } finally {
      setLoading(false);
    }
  }

  const confirmCode = async (code: string) => {
    try {
      setLoading(true);
      if (!await account.confirmSignUp(code)) {
        toast.error('Confirmation failed');
        return ConfirmResult.Unknown;
      }
      return ConfirmResult.Success;
    } catch (err) {
      if (err.code == 'CodeMismatchException') {
        return ConfirmResult.CodeMismatch;
      } else {
        toast.error(`${err.message} (${err.code})`);
        return ConfirmResult.Unknown;
      }
    } finally {
      setLoading(false);
    }
  }

  const completeNewPasswordChallenge = async (newPassword: string, locale: string) => {
    try {
      setLoading(true);
      await account.completeNewPasswordChallenge(newPassword, locale);
    } catch (err) {
      toast.error(err);
    } finally {
      setLoading(false);
    }
  }

  const changePassword = async (prevPassword: string, newPassword: string) => {
    try {
      setLoading(true);
      await account.changePassword(prevPassword, newPassword);
    } catch (err) {
      toast.error(err);
    } finally {
      setLoading(false);
    }
  }

  const refreshCredentials = async () => {
    try {
      setLoading(true);
      await account.refreshCredentials();
    } catch (err) {
      toast.error(err);
    } finally {
      setLoading(false);
    }
  }

  return {
    resendConfirmationCode, logIn, logOut, signUp,
    changePassword, sendRecoveryEmail, resetPassword, confirmCode,
    completeNewPasswordChallenge, user, refreshCredentials
  };
}

export const AccountProvider = (props: { region: string, children?: React.ReactNode }) => {
  const [ready, setReady] = React.useState(false);
  const [loading, setLoading] = useState(false);
  const [mode, setMode] = useState(AccountMode.LoggedOut);
  const [user, setUser] = React.useState<AccountUser>();

  React.useEffect(() => {
    setLoading(true);
    _account.init(props.region)
      .then(async () => {
        const user = await _account.getCurrentUser();
        setMode(user ? AccountMode.LoggedIn : AccountMode.LoggedOut);
        setReady(true);
      })
      .catch(console.error)
      .finally(() => setLoading(false))
  }, []);

  React.useEffect(() => {
    if (ready) {
      setLoading(true);
      _account.getCurrentUser()
        .then(async user => {
          if (!user) setUser(undefined);
          else setUser(user);
        })
        .catch(console.error)
        .finally(() => setLoading(false));
    }
  }, [ready, mode]);

  if (!ready) return null;

  return (
    <AccountContext.Provider value={{
      account: _account,
      setMode, user,
    }}>
      {props.children}
    </AccountContext.Provider>
  );
}