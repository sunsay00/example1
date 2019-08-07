import * as React from 'react';
import { useState, useContext } from 'react';
import { AsyncStorage } from 'react-native';
import { Account, UserPoolMode } from 'cf-cognito';
import * as UI from 'gatsby-theme-core-ui';

export enum LogInResult { Success, ChangePassword, UserNotFound, NotAuthorized, UserNotConfirmed, Unknown };
export enum SignUpResult { Success, UsernameExists, Unknown };
export enum ConfirmResult { Success, CodeMismatch, Unknown };

export const useAccount = () => {
  const toast = UI.useToast();
  const [loading, setLoading] = useState(false);
  const account = useContext(AccountContext);

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

  const logIn = async (emailOrUsername: string, password: string): Promise<LogInResult> => {
    try {
      setLoading(true);
      const result = await account.signIn(emailOrUsername, password);
      if (result == 'changepassword') {
        return LogInResult.ChangePassword;
      } else {
        return LogInResult.Success;
      }
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

  const confirm = async (code: string) => {
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

  const changePassword = async (newPassword: string, locale: string) => {
    try {
      setLoading(true);
      await account.completeNewPasswordChallenge(newPassword, locale);
    } catch (err) {
      toast.error(err);
    } finally {
      setLoading(false);
    }
  }

  return { loading, resendConfirmationCode, logIn, signUp, sendRecoveryEmail, resetPassword, confirm, changePassword };
}

const _account = new Account(UserPoolMode.Web, AsyncStorage);

const AccountContext = React.createContext<Account>(_account);

export const AccountProvider = (props: { region: string, children?: React.ReactNode }) => {
  const [ready, setReady] = React.useState(false);

  React.useEffect(() => {
    _account.init(props.region)
      .then(() => setReady(true))
      .catch(console.error);
  }, []);

  if (!ready) return <UI.Loading />;

  return <AccountContext.Provider value={_account}>{props.children}</AccountContext.Provider>;
}