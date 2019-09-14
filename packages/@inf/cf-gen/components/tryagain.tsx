import * as React from 'react';
import * as UI from '@inf/core-ui';

export const TryAgain = (props: {
  error?: Error | string,
  onRetry: () => Promise<void>,
}) => {
  const { loading, setLoading } = UI.useLoading(TryAgain);

  const toast = UI.useToast();

  const onRetry = async () => {
    try {
      setLoading(true);
      await props.onRetry();
    } catch (err) {
      toast.error(err);
    } finally {
      setLoading(false);
    }
  }

  if (loading) {
    return <UI.ActivityIndicator />;
  } else {
    return (
      <UI.View>
        <UI.Text>{props.error == undefined ? 'Something broke' : (props.error instanceof Error) ? props.error.message : props.error}</UI.Text>
        <UI.Button
          disabled={loading}
          onPress={() => onRetry()} // lambda reqired here (issue: https://github.com/apollographql/apollo-client/issues/1217)
        >Retry?</UI.Button>
      </UI.View >
    );
  }
}