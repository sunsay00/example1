import { useState } from 'react';

const useToast = () => {
  return {
    info: (msg: string) => {
      console.log(msg);
    },
    error: (msg: string) => {
      console.error(msg);
    },
  };
}

export const useAccount = () => {
  const toast = useToast();
  const [loading, setLoading] = useState(false);
  const resendConfirmation = async (username: string) => {
    /*
    try {
      setLoading(true);
      await Root.account().resendConfirmationCode();
      this.props.onNeedsConfirmation(username);
    } catch (err) {
      Toast.error(err);
    } finally {
      setLoading(false);
    }
    */
  }

  const logIn = async (emailOrUsername: string, password: string) => {
    /*
    try {
      setLoading(true);
      await this.props.onContinue(emailOrUsername.trim(), password);
    } catch (err) {
      if (err.code === 'UserNotFoundException' || err.code === 'NotAuthorizedException') {
        toast.info(err.message);
      } else if (err.code === 'UserNotConfirmedException') {
        UI.Alert.alert('User not confirmed', 'This user has not been confirmed, resend confirmation code?', [
          { text: 'Cancel', onPress: () => { } },
          { text: 'Resend', onPress: () => this.onResendConfirmation(values.emailOrUsername) },
        ])
      } else {
        toast.info(`${err.message} (${err.code})`);
      }
    } finally {
      setLoading(false);
    }
    */
  };

  const signUp = async (username: string, email: string, password: string) => {
  }

  const sendRecoveryEmail = async (emailOrUsername: string) => {
  }

  const resetPassword = async (emailOrUsername: string, code: string, newPassword: string) => {
    /*
    try {
      this.setState({ loading: true });
      await Root.account().confirmForgotPassword(this.props.username, values.code, values.newPassword);
      Toast.info('Password has successfully been reset');
      await this.props.onComplete(this.state.newPassword);
    } catch (err) {
      Toast.error(err);
    } finally {
      this.setState({ loading: false, newPassword: '', code: '' });
    }
    */
  }

  return { loading, resendConfirmation, logIn, signUp, sendRecoveryEmail, resetPassword };
}
