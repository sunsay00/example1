import { useToast } from './usetoast';
import { useAccount, SignUpResult, LogInResult, ConfirmResult } from './useaccount';
import { useForm } from 'core-hooks';
import { Alert } from '../components/alert';

export const useLogInForm = (props: {
  onLogInComplete: () => void,
  onGoToChangePassword: () => void,
  onGoToConfirmation: (verifiedUsername: string) => void,
}) => {
  const { loading, logIn, resendConfirmationCode } = useAccount();
  const toast = useToast();
  const form = useForm({
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
      Alert.alert('Unconfirmed user', 'This user has not been confirmed, resend confirmation code?', [
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

  return { loading, form };
}

export const useChangePasswordForm = (props: { locale: string }) => {
  const { loading, changePassword } = useAccount();
  const form = useForm({
    newPassword: {
      type: 'password',
      placeholder: 'New Password',
      message: 'Password too short',
      pattern: /^.{8,}$/,
      default: '',
    },
  }, async ({ newPassword }) => {
    await changePassword(newPassword, props.locale);
  });

  return { loading, form };
}

export const useConfirmationForm = (props: { onGoToLogIn: () => void }) => {
  const { loading, resendConfirmationCode, confirmCode } = useAccount();
  const toast = useToast();

  const form = useForm({
    code: {
      type: 'text',
      placeholder: 'Confirmation Code',
      message: 'Invalid code',
      pattern: /.+/,
      default: '',
    }
  }, async ({ code }) => {
    const result = await confirmCode(code);
    if (result == ConfirmResult.Success) {
      toast.info('Your account has been successfully confirmed', () =>
        props.onGoToLogIn());
    } else if (result == ConfirmResult.CodeMismatch) {
      toast.warn('Your confirmation code did not match our records');
    } else if (result == ConfirmResult.Unknown) {
      throw new Error('unknown signup error');
    } else {
      throw new Error(`unhandled signup error (${result})`);
    }
  });

  return { loading, form, resendConfirmationCode };
}

export const useResetPasswordForm = (props: { emailOrUsername: string, onGoToLogIn: () => void }) => {
  const { loading, resetPassword } = useAccount();

  const form = useForm({
    code: {
      type: 'text',
      placeholder: 'Code',
      message: 'Invalid code',
      pattern: /.+/,
      default: '',
    },
    newPassword: {
      type: 'password',
      placeholder: 'New Password',
      message: 'Password too short',
      pattern: /^.{8,}$/,
      default: '',
    },
  }, async ({ code, newPassword }) => {
    await resetPassword(props.emailOrUsername, code, newPassword);
    props.onGoToLogIn();
  });

  return { loading, form };
}

export const useSignUpForm = (props: {
  onGoToConfirmation: (verifiedUsername: string) => void,
  role: string,
  locale: string,
}) => {
  const { loading, signUp } = useAccount();
  const toast = useToast();

  const form = useForm({
    username: {
      type: 'username',
      placeholder: 'Username',
      message: 'Invalid Username',
      default: '',
    },
    email: {
      type: 'email',
      placeholder: 'Email',
      message: 'Invalid Email address',
      default: '',
    },
    password: {
      type: 'password',
      placeholder: 'Password',
      pattern: /^.{8,}$/,
      message: 'Password too short',
      default: '',
    },
  }, async ({ username, email, password }) => {
    const result = await signUp(username, email, password, props.locale, props.role);
    if (result == SignUpResult.Success) {
      props.onGoToConfirmation(username);
    } else if (result == SignUpResult.UsernameExists) {
      toast.warn('User already exists');
    } else if (result == SignUpResult.Unknown) {
      throw new Error('unknown signup error');
    }
  });

  return { loading, form };
}

export const useForgotPasswordForm = (props: {
  onGoToResetPassword: (emailOrUsername: string) => void,
}) => {
  const { loading, sendRecoveryEmail } = useAccount();
  const form = useForm({
    emailOrUsername: {
      type: 'text',
      pattern: /^.{2,}$/,
      placeholder: 'Email or Username',
      message: 'Invalid Email or Username',
      default: '',
    },
  }, async ({ emailOrUsername }) => {
    await sendRecoveryEmail(emailOrUsername);
    props.onGoToResetPassword(emailOrUsername);
  });

  return { loading, form };
}