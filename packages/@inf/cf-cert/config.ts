import { createConfigRecord } from '@inf/vars/configure';

export const Config = (inputs: {
  Domain: string,
}) => createConfigRecord({
  type: 'cloudformation',
  rootDir: __dirname,
  cfpath: './cf.yaml',
  inputs,
  outputs: {
    CertificateArn: ''
  }
});
