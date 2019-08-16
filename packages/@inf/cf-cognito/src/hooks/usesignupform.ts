import * as UI from '@inf/core-ui';
import { useAccount, SignUpResult } from './useaccount';

export const useSignUpForm = (props: {
  onGoToConfirmation: (verifiedUsername: string) => void,
  role: string,
  locale: string,
}) => {
  const { loading } = UI.useLoading(useSignUpForm);
  const { signUp } = useAccount();
  const toast = UI.useToast();

  const form = UI.useForm({
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