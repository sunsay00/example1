import { useState, useContext } from 'react';
import { useToast } from './usetoast';
import { AccountContext } from '../components/accountprovider';

export enum LogInResult { Success, ChangePassword, UserNotFound, NotAuthorized, UserNotConfirmed, Unknown };

export const useAccount = () => {
  const toast = useToast();
  const [loading, setLoading] = useState(false);
  const account = useContext(AccountContext);

  const resendConfirmationCode = async () => {
    try {
      setLoading(true);
      await account.resendConfirmationCode();
      toast.success('A confirmation code has been resent');
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
      if (err.code === 'UserNotFoundException') {
        return LogInResult.UserNotFound;
      } else if (err.code === 'NotAuthorizedException') {
        return LogInResult.NotAuthorized;
      } else if (err.code === 'UserNotConfirmedException') {
        return LogInResult.UserNotConfirmed;
      } else {
        toast.info(`${err.message} (${err.code})`);
        return LogInResult.Unknown;
      }
    } finally {
      setLoading(false);
    }
  };

  const signUp = async (username: string, email: string, password: string, locale: string, role: string) => {
    try {
      setLoading(true);
      await account.signUp(username, email, password, locale, role);
    } catch (err) {
      toast.error(err);
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

  const confirmSignUp = async (code: string) => {
    try {
      setLoading(true);
      if (!await account.confirmSignUp(code))
        toast.error('Confirmation failed');
    } catch (err) {
      toast.error(err);
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

  return { loading, resendConfirmationCode, logIn, signUp, sendRecoveryEmail, resetPassword, confirmSignUp, changePassword };
}
