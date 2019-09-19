import { Cursorize } from '@inf/cf-gen';
import { IStore, IDeviceListsPartialService } from '../../_gen';
import NotificationManager from '../tools/notificationmanager';
import * as M from '../../_gen';
import { UserContext } from '../types';

export default class DeviceListsService implements IDeviceListsPartialService<UserContext> {
  private _notifications: NotificationManager;
  private store: IStore<UserContext>;

  constructor(notifications: NotificationManager, store: IStore<UserContext>) {
    this._notifications = notifications;
    this.store = store;
  }

  private subscribeToObjects = async (chatIdentity: string) => {
    // TODO: implement me, the device has been registered for notifications,
    // invoke `this._notifications.subscribe(this.store, chatIdentity, '<model-name>', '<model-id>');` 
    // to receive notifications on objects `sub` wants notifications on
  }

  register = async ($ctx: UserContext, token: string, userAgent: string, chatIdentity: string) => {
    let deviceList = await this.store.deviceListsFindByChatIdentity($ctx, chatIdentity);
    if (deviceList == undefined) {
      const endpoint = await this._notifications.registerDevice(userAgent, chatIdentity, undefined, token);
      const device: M.Device = { token, endpoint, createdAt: new Date(), userAgent };
      await this.store.deviceListsCreate($ctx, chatIdentity, [device]);
      await this.subscribeToObjects(chatIdentity);
      return device;
    } else {
      const device = deviceList.devices.find(d => d.token == token);
      if (device == undefined) {
        const endpoint = await this._notifications.registerDevice(userAgent, chatIdentity, undefined, token);
        const device: M.Device = { token, endpoint, createdAt: new Date(), userAgent };
        await this.store.deviceListsUpdate($ctx, deviceList.id, [device, ...deviceList.devices]);
        await this.subscribeToObjects(chatIdentity);
        return device;
      } else {
        // if an endpoint has been enabled/disabled, it needs to be refreshed
        const endpoint = await this._notifications.registerDevice(userAgent, chatIdentity, device.endpoint, token);
        if (endpoint == device.endpoint) {
          await this.subscribeToObjects(chatIdentity);
          return device;
        } else {
          const device: M.Device = { token, endpoint, createdAt: new Date(), userAgent };
          await this.store.deviceListsUpdate($ctx, deviceList.id, deviceList.devices.map(d => d.endpoint == device.endpoint ? device : d));
          await this.subscribeToObjects(chatIdentity);
          return device;
        }
      }
    }
  }

  myDevices = async ($ctx: UserContext, __fields: string[], chatIdentity: string): Promise<(M.Device & Cursorize)[]> => {
    let deviceList = await this.store.deviceListsFindByChatIdentity($ctx, chatIdentity);
    return deviceList == undefined ? [] : deviceList.devices;
  }

  unregister = async ($ctx: UserContext, token: string, chatIdentity: string): Promise<M.Device | undefined> => {
    let deviceList = await this.store.deviceListsFindByChatIdentity($ctx, chatIdentity);
    if (deviceList == undefined) {
      return undefined;
    } else {
      const device = deviceList.devices.find(d => d.token == token);
      if (device == undefined) {
        return undefined;
      } else {
        await this._notifications.unregisterDevice(chatIdentity, device.endpoint);
        let devices = deviceList.devices.filter(d => d.token != token);
        if (devices.length == 0) {
          await this.store.deviceListsRemove($ctx, deviceList.id);
        } else {
          await this.store.deviceListsUpdate($ctx, deviceList.id, devices);
        }
        return device;
      }
    }
  }
}