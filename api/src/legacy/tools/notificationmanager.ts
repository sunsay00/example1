import AWSClient from './awsclient';
import { INotificationManager, IStore, ICacheClient } from '@inf/cf-gen';
import * as M from '@inf/cf-gen/model';
import { UserContext } from '../types';

export default class NotificationManager implements INotificationManager<UserContext> {
  private _aws: AWSClient;
  private _cache: ICacheClient | undefined;
  constructor(params: { stage: string, region: string, platformApplicationArn: string, cache?: ICacheClient }) {
    this._aws = new AWSClient(params.stage, params.region, params.platformApplicationArn, params.cache);
    this._cache = params.cache;
  }

  async dispatchDeckFactories(store: IStore<UserContext>, sub: string, action: string, obj: M.DeckFactory) { }
  async dispatchDecks(store: IStore<UserContext>, sub: string, action: string, obj: M.Deck) { }
  async dispatchCommentLists(store: IStore<UserContext>, sub: string, action: string, obj: M.CommentList) { }
  async dispatchDealerships(store: IStore<UserContext>, sub: string, action: string, obj: M.Dealership) { }
  async dispatchCarInventories(store: IStore<UserContext>, sub: string, action: string, obj: M.CarInventory) { }
  async dispatchSellers(store: IStore<UserContext>, sub: string, action: string, obj: M.Seller) { }
  async dispatchNotes(store: IStore<UserContext>, sub: string, action: string, obj: M.Note) { }
  async dispatchCarInfos(store: IStore<UserContext>, sub: string, action: string, obj: M.CarInfo) { }

  async subscribe($user: UserContext, store: IStore<UserContext>, chatIdentity: string, typename: string, id: string) {
    const name = `${typename}-${id}`;
    const topicArn = await this._aws.findOrCreateTopic(name);
    const devicelist = await store.deviceListsFindByChatIdentity($user, chatIdentity);
    if (devicelist != undefined) {
      for (let key in devicelist.devices) {
        const device = devicelist.devices[key];
        await this._aws.subscribeToTopic(topicArn, device.endpoint);
      }
    }
  }

  async unsubscribe($user: UserContext, store: IStore<UserContext>, chatIdentity: string, typename: string, id: string) {
    const name = `${typename}-${id}`;
    //const topicArn = 
    await this._aws.findOrCreateTopic(name);
    const devicelist = await store.deviceListsFindByChatIdentity($user, chatIdentity);
    if (devicelist != undefined) {
      let devices = [...devicelist.devices];
      let found = await this._aws.findSubscriptionsByTopic(name);
      while (devices.length > 0) {
        for (let key in found.subscriptions) {
          const sub = found.subscriptions[key];
          if (devices.findIndex(d => d.endpoint == sub.endpoint) != -1) {
            await this._aws.unsubscribeFromTopic(sub.subscriptionArn);
            devices = devices.filter(d => d.endpoint != sub.endpoint);
            if (devices.length == 0) {
              break;
            }
          }
        }
        if (found.cursor != undefined) {
          found = await this._aws.findSubscriptionsByTopic(name, found.cursor);
        } else {
          break;
        }
      }
    }
  }

  /*async publishToBuilding(buildingId: string, type: string, id: string, message: string) {
    const name = `buildings-${buildingId}`;
    const topicArn = await this._aws.findOrCreateTopic(name);
    const custom = { type, id, parentType: 'buildings', parentId: buildingId };
    await this._aws.publish(topicArn, message, custom);
  }*/

  async sendToUser($user: UserContext, store: IStore<UserContext>, chatIdentity: string, type: string, id: string, message: string) {
    const list = await store.deviceListsFindByChatIdentity($user, chatIdentity);
    if (list != undefined) {
      const custom = { type, id, parentType: '', parentId: '' };
      for (let key in list.devices) {
        const device = list.devices[key];
        await this._aws.send(device.endpoint, message, custom);
      }
    }
  }

  registerDevice = async (userAgent: string, chatIdentity: string, endpoint: string | undefined, deviceToken: string): Promise<string> => {
    if (endpoint == undefined) {
      const retEndpoint = await this._aws.registerDevice(userAgent, endpoint, deviceToken, chatIdentity);
      this._cache && await this._cache.set(`endpoints.chatIdentity[${chatIdentity}]`, retEndpoint);
      return retEndpoint;
    } else {
      const prevEndpoint = this._cache && await this._cache.get(`endpoints.chatIdentity[${chatIdentity}]`);
      if (prevEndpoint == endpoint) {
        return prevEndpoint;
      } else {
        const retEndpoint = await this._aws.registerDevice(userAgent, endpoint, deviceToken, chatIdentity);
        this._cache && await this._cache.set(`endpoints.chatIdentity[${chatIdentity}]`, retEndpoint);
        return retEndpoint;
      }
    }
  }
  unregisterDevice = async (chatIdentity: string, endpoint: string): Promise<void> => {
    await this._aws.unregisterDevice(endpoint);
    this._cache && await this._cache.del([`endpoints.chatIdentity[${chatIdentity}]`]);
  }
}
