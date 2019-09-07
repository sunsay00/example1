import * as AWS from 'aws-sdk';
import vars from '../../_vars';
import { ICacheClient } from '@inf/cf-gen';
import * as M from '@inf/cf-gen/model';

let _sns: AWS.SNS | undefined = undefined;
let _cisp: AWS.CognitoIdentityServiceProvider;

const getSns = (region: string): AWS.SNS => {
  if (_sns == undefined) {
    _sns = new AWS.SNS({ apiVersion: '2010-03-31', region });
  }
  return _sns;
}

const getCisp = (): AWS.CognitoIdentityServiceProvider => {
  if (_cisp == undefined) {
    AWS.config.update({ region: vars.AWS_REGION });
    _cisp = new AWS.CognitoIdentityServiceProvider({ apiVersion: '2016-04-18' });
  }
  return _cisp;
};

const createEndpoint = async (region: string, PlatformApplicationArn: string, userAgent: string, deviceToken: string, customUserData: string): Promise<string> => {
  let endpointArn: undefined | string = undefined;

  let platform: 'ios' | 'android' | undefined;
  if (userAgent.toLowerCase().search('ios') != -1) {
    platform = 'ios';
  } else if (userAgent.toLowerCase().search('android') != -1) {
    platform = 'android';
  }

  if (platform == undefined) {
    throw new Error(`failed to create device endpoint - unrecognized platform for ${userAgent}`);
  }

  try {
    console.log(`Creating endpoint with token ${deviceToken} userAgent:${userAgent}`);

    const params = {
      //PlatformApplicationArn: config(platform == 'ios' ? 'AWS_SNS_APP' : 'AWS_SNS_APP_ANDROID'),
      PlatformApplicationArn,
      CustomUserData: customUserData,
      Token: deviceToken,
    };
    const result = await getSns(region).createPlatformEndpoint(params).promise();
    endpointArn = result.EndpointArn;
  } catch (err) {
    console.info(err);
    if (err.code == 'InvalidParameter') {
      console.error(`exception message ${err.message}`);
      const matches = /.*Endpoint (arn:aws:sns[^ ]+) already exists with the same Token.*/.exec(`${err.message}`);
      if (matches != undefined && matches.length >= 2) {
        endpointArn = matches[1];
      } else {
        throw err;
      }
    } else {
      throw err;
    }
  }

  if (endpointArn == undefined) {
    throw new Error('failed to retrieve endpoint arn');
  }

  return endpointArn;
};

export default class AWSClient {
  private _stage: string;
  private _region: string;
  private _platformApplicationArn: string;
  private _cache: ICacheClient | undefined;
  constructor(stage: string, region: string, platformApplicationArn: string, cache?: ICacheClient) {
    this._stage = stage;
    this._region = region;
    this._platformApplicationArn = platformApplicationArn;
    this._cache = cache;
  }
  findOrCreateTopic = async (name: string): Promise<string> => {
    const cret = this._cache && await this._cache.get(`sns.topic[${name}]`);
    if (cret != undefined) {
      return cret;
    } else {
      const ret = await getSns(this._region).createTopic({
        Name: `${this._stage}-${name}`,
      }).promise();
      if (ret.TopicArn == undefined) {
        throw new Error('failed to create topic');
      }
      this._cache && await this._cache.set(`sns.topic[${name}]`, ret.TopicArn);
      return ret.TopicArn;
    }
  }
  deleteTopic = async (topicArn: string) => {
    await getSns(this._region).deleteTopic({
      TopicArn: topicArn
    }).promise();
  }
  subscribeToTopic = async (topicArn: string, endpoint: string): Promise<string> => {
    const ret = await getSns(this._region).subscribe({
      Endpoint: endpoint,
      Protocol: 'application',
      TopicArn: topicArn,
    }).promise();
    if (ret.SubscriptionArn == undefined) {
      throw new Error('failed to subscribe to topic');
    }
    return ret.SubscriptionArn;
  }
  findSubscriptionsByTopic = async (topicArn: string, cursor?: string): Promise<{ cursor?: string, subscriptions: { endpoint: string, subscriptionArn: string }[] }> => {
    let ret = [] as { endpoint: string, subscriptionArn: string }[];
    const rslt = await getSns(this._region).listSubscriptionsByTopic({
      TopicArn: topicArn,
      NextToken: cursor,
    }).promise();
    if (rslt.Subscriptions == undefined) {
      return { subscriptions: ret };
    } else {
      for (let i = 0; i < rslt.Subscriptions.length; ++i) {
        var sub = rslt.Subscriptions[i];
        if (sub.Endpoint != undefined && sub.SubscriptionArn != undefined) {
          ret.push({
            endpoint: sub.Endpoint,
            subscriptionArn: sub.SubscriptionArn
          });
        }
      }
      return { cursor: rslt.NextToken, subscriptions: ret };
    }
  }
  unsubscribeFromTopic = async (subscriptionArn: string) => {
    await getSns(this._region).unsubscribe({
      SubscriptionArn: subscriptionArn
    }).promise();
  }
  publish = async (topicArn: string, message: string, custom: unknown): Promise<string> => {
    const apns = JSON.stringify({
      ...custom,
      aps: { alert: message, sound: 'default', badge: 0 },
    });

    const isProd = this._stage == 'production';

    const payload = {
      ...(isProd ? { APNS: apns } : { APNS_SANDBOX: apns }),
      GCM: JSON.stringify({ data: { ...custom, message } }),
      default: message,
    };

    const ret = await getSns(this._region).publish({
      MessageStructure: 'json',
      Message: JSON.stringify(payload),
      TopicArn: topicArn,
    }).promise();
    if (ret.MessageId == undefined)
      throw new Error('bad publish message response');
    return ret.MessageId;
  }

  send = async (targetArn: string, message: string, custom: unknown) => {
    const apns = JSON.stringify({
      ...custom,
      aps: { alert: message, sound: 'default', badge: 0 },
    });

    const isProd = this._stage == 'production';

    const payload = {
      ...(isProd ? { APNS: apns } : { APNS_SANDBOX: apns }),
      GCM: JSON.stringify({ data: { ...custom, message } }),
      default: message,
    };

    const ret = await getSns(this._region).publish({
      MessageStructure: 'json',
      Message: JSON.stringify(payload),
      TargetArn: targetArn,
    }).promise();
    if (ret.MessageId == undefined) {
      throw new Error('bad send message response');
    }
    return ret.MessageId;
  }
  unregisterDevice = async (endpoint: string): Promise<void> => {
    await getSns(this._region).deleteEndpoint({
      EndpointArn: endpoint,
    }).promise();
  }
  registerDevice = async (userAgent: string, endpoint: string | undefined, deviceToken: string, customUserData: string): Promise<string> => {
    let endpointArn = endpoint;

    let updateNeeded = false;
    let createNeeded = true;

    if (endpointArn == undefined) {
      // No endpoint ARN is stored; need to call CreateEndpoint
      endpointArn = await createEndpoint(this._region, this._platformApplicationArn, userAgent, deviceToken, customUserData);
      createNeeded = false;
    }

    console.log("Retrieving endpoint data...");
    // Look up the endpoint and make sure the data in it is current, even if
    // it was just created

    try {
      const data: any = await getSns(this._region).getEndpointAttributes({
        EndpointArn: endpointArn
      }).promise();
      updateNeeded = data.Token == deviceToken || !data.Enabled;
    } catch (err) {
      console.info(err);
      if (err.code == 'NotFound') {
        createNeeded = true;
      } else {
        throw err;
      }
    }

    if (createNeeded) {
      endpointArn = await createEndpoint(this._region, this._platformApplicationArn, userAgent, deviceToken, customUserData);
    }

    console.log("updateNeeded=" + updateNeeded);

    if (updateNeeded) {
      // endpoint is out of sync with the current data;
      // update the token and enable it.
      console.log(`Updating endpoint ${endpointArn}`);
      const params: any = {
        Attribures: {
          Token: deviceToken,
          Enabled: true,
        },
        EndpointArn: endpointArn,
      };
      await getSns(this._region).setEndpointAttributes(params);
    }

    return endpointArn;
  }
};

const padLeft = (str: string, len: number, fill: string = ' ') =>
  str.length >= len ? str : (new Array(Math.ceil((len - str.length) / fill.length) + 1).join(fill)).substr(0, (len - str.length)) + str;

export const inviteUser = async (username: string, email: string, locale: string, role: string, uservar1: string, action: 'RESEND' | 'SUPRESS' | undefined): Promise<M.PendingSeller | undefined> => {
  const cisp = getCisp();
  const poolId = vars.UserPoolId;

  // adminCreateUser throws UsernameExistsException even when the user does not exist,
  // as a workaround we check if the user doesn't exist with adminGetUser and ignore the exception

  /*
  var found = false;
  try {
    const user = await cisp.adminGetUser({
      Username: username,
      UserPoolId: poolId,
    }).promise();
    console.log('::::', JSON.stringify(user));
    found = true;
  } catch (err) {
    console.log('BBBB', JSON.stringify(err));
    // ignoring exception
  }

  if (found) {
    throw new Error('username unavailable');
  }
  */

  try {
    const splitedName = username.split(' ');
    let login = splitedName.length > 1 ? splitedName[0] + '.' + splitedName[splitedName.length - 1] : username;
    if (action == 'RESEND') {
      login = username;
    } else {
      const listUsersResponse = await cisp.listUsers({
        UserPoolId: poolId,
        Filter: 'username ^= "' + login + '"',
      }).promise();
      var regex = new RegExp(login + '+' + '[1-9]', 'i');
      const found = listUsersResponse.Users && listUsersResponse.Users.filter(u => u.Username
        && (u.Username.match(regex)
          || u.Username == login));

      if (found != undefined && found.length > 0) {
        login = login + found.length.toString();
      }
      console.log('login to be created: ' + login);
    }
    const response = await cisp.adminCreateUser({
      DesiredDeliveryMediums: ['EMAIL'],
      ForceAliasCreation: true,
      MessageAction: action,
      TemporaryPassword: padLeft(`${Math.ceil(Math.random() * 1000000001) + 1}`, 8, '0'),
      UserAttributes: [
        { Name: 'email', Value: email },
        { Name: 'email_verified', Value: 'True' },
        { Name: 'locale', Value: locale },
        { Name: 'family_name', Value: uservar1 },
        { Name: 'preferred_username', Value: login },
        { Name: 'custom:role', Value: role },
        { Name: 'custom:uservar1', Value: uservar1 },
        { Name: 'custom:uservar2', Value: username },
      ],
      Username: login,
      UserPoolId: poolId,
    }).promise();
    if (action == 'RESEND') {
      console.log('temp password resend for ' + login);
    } else {
      console.log('created ' + login);
    }
    if (response.User != undefined) {
      const sub = response.User.Attributes && response.User.Attributes.find(a => a.Name == 'sub');
      const email = response.User.Attributes && response.User.Attributes.find(a => a.Name == 'email');
      const name = response.User.Attributes && response.User.Attributes.find(a => a.Name == 'custom:uservar2');
      const username = response.User.Username;
      if (sub && sub.Value && email && email.Value && name && name.Value && username) {
        return {
          sub: sub.Value,
          username: username,
          email: email.Value,
          name: name.Value
        }
      } else {
        return undefined;
      }
    } else {
      return undefined;
    }
  } catch (err) {
    console.log(`error creating user ${username}`, err);
    throw err;
  }
};

export const adminDeleteUser = async (username: string) => {
  const cisp = getCisp();
  await cisp.adminDeleteUser({
    UserPoolId: vars.UserPoolId,
    Username: username,
  }).promise();
}

export const adminEnableDisableUser = async (sub: string, dealershipId: string, disableUser: boolean) => {
  const users = await adminListUsers('CONFIRMED', dealershipId);
  const found = users.find(u => u.sub == sub);
  if (found != undefined) {
    const cisp = getCisp();
    disableUser
      ? await cisp.adminDisableUser({
        UserPoolId: vars.UserPoolId,
        Username: found.username != undefined ? found.username : '',
      }).promise()
      : await cisp.adminEnableUser({
        UserPoolId: vars.UserPoolId,
        Username: found.username != undefined ? found.username : '',
      }).promise();
  } else {
    console.log('Cannot find user for ' + sub);
    throw new Error('Invalid user');
  }
}

export const adminConfirmSignUp = async (username: string) => {
  const cisp = getCisp();
  await cisp.adminConfirmSignUp({
    UserPoolId: vars.UserPoolId,
    Username: username,
  }).promise();
}

export const adminAddUserToGroup = async (username: string, group: string) => {
  const cisp = getCisp();
  await cisp.adminAddUserToGroup({
    UserPoolId: vars.UserPoolId,
    Username: username,
    GroupName: group,
  }).promise();
}

export const adminListUsers = async (filter: string, dealershipId: string, limit?: number, token?: string): Promise<{ sub: string, username: string, email: string, name: string, token?: string }[]> => {
  const cisp = getCisp();
  const listUsersResponse = await cisp.listUsers({
    UserPoolId: vars.UserPoolId,
    Filter: 'family_name = "' + dealershipId + '" and cognito:user_status = "' + filter + '"',
    Limit: limit,
    PaginationToken: token
  }).promise();
  if (listUsersResponse.Users != undefined) {
    const filteredUsers = listUsersResponse.Users.filter(u => u.Attributes
      && u.Attributes.find(a => a.Name == 'custom:uservar1'
        && a.Value != undefined && a.Value == dealershipId) != undefined);
    if (filteredUsers.length > 0) {
      let users: { sub: string, username: string, email: string, name: string, token?: string }[];
      users = [];
      filteredUsers.map((u) => {
        const username = u.Username;
        const sub = u.Attributes && u.Attributes.find(a => a.Name == 'sub');
        const email = u.Attributes && u.Attributes.find(a => a.Name == 'email');
        const name = u.Attributes && u.Attributes.find(a => a.Name == 'custom:uservar2');
        if (username != undefined && sub != undefined && sub.Value != undefined && email != undefined && email.Value != undefined) {
          users.push({
            sub: sub.Value,
            username: username,
            email: email.Value,
            name: name != undefined && name.Value != undefined ? name.Value : username,
            token: listUsersResponse.PaginationToken
          });
        }
      });
      return users;
    } else {
      return [];
    }
  } else {
    return [];
  }
}
