import * as UI from 'core-ui';
import { useAccount, LogInResult } from './useaccount';

export const useLogInForm = (props: {
  onLogInComplete: () => void,
  onGoToChangePassword: () => void,
  onGoToConfirmation: (verifiedUsername: string) => void,
}) => {
  const { logIn, resendConfirmationCode } = useAccount();

  const toast = UI.useToast();
  const form = UI.useForm({
    emailOrUsername: {
      type: 'text',
      placeholder: 'Email or Username',
      pattern: /^.{2,}$/,
      message: 'Invalid Email or Username',
      default: '',
    },
    password: {
      type: 'password',
      pattern: /^.{8,}$/,
      placeholder: 'Password',
      message: 'Password too short',
      default: '',
    },
  }, async ({ emailOrUsername, password }) => {
    const result = await logIn(emailOrUsername, password);
    if (result == LogInResult.Success) {
      props.onLogInComplete();
    } else if (result == LogInResult.ChangePassword) {
      props.onGoToChangePassword();
    } else if (result == LogInResult.UserNotFound) {
      toast.warn('User not found');
    } else if (result == LogInResult.NotAuthorized) {
      toast.warn('User not authorized');
    } else if (result == LogInResult.UserNotConfirmed) {
      UI.Alert.alert('Unconfirmed user', 'This user has not been confirmed, resend confirmation code?', [
        { text: 'Cancel', onPress: () => { } },
        {
          text: 'Resend', onPress: async () => {
            await resendConfirmationCode();
            props.onGoToConfirmation(emailOrUsername);
          }
        },
      ])
    } else if (result == LogInResult.Unknown) {
      throw new Error('unknown signup error');
    } else {
      throw new Error(`unhandled signup error (${result})`);
    }
  });

  return { form };
}

