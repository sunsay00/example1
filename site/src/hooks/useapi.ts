import { useMutation } from 'react-apollo-hooks';
import gql from 'graphql-tag';
import * as GQL from '../graphql';

const MULTIPART_UPLOAD = gql`
mutation ($command: String!, $payload: String!) {
  api { multipartUpload(command: $command, payload: $payload) }
}
`;

export const useApi = () => {
  const multipartUpload = useMutation<Mutation<'api'>, GQL.ApiMultipartUploadArgs>(MULTIPART_UPLOAD);
  return {
    multipartUpload: async (command: string, payload: string) => {
      const ret = await multipartUpload({ variables: { command, payload } });
      if (!ret.data)
        throw new Error('invalid multipoartupload result');
      return ret.data.api.multipartUpload;
    }
  };
}
