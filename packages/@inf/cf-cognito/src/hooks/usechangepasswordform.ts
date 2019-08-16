import * as UI from '@inf/core-ui';
import { useAccount } from './useaccount';

export const useChangePasswordForm = (props: { locale: string }) => {
  const { loading } = UI.useLoading(useChangePasswordForm);
  const { changePassword } = useAccount();
  const form = UI.useForm({
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