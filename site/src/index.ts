import { Account } from 'cf-cognito';
import { AsyncStorage } from 'react-native';

const account = new Account('us-east-1', 'Web', AsyncStorage);

const main = async () => {
  await account.init();
  console.log('ready');
}

main().catch(err => console.error(err.message || err));