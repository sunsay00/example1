import { IStore, IUsersPartialService } from '@inf/cf-gen';
import * as M from '@inf/cf-gen/model';
import { UserContext } from '../types';
import NotificationManager from '../tools/notificationmanager';

export default class UsersService implements IUsersPartialService<UserContext> {
  private _store: IStore<UserContext>;
  private _locale: string;
  private _region: string;

  constructor(notifications: NotificationManager, store: IStore<UserContext>, locale: string, region: string) {
    this._store = store;
    this._locale = locale;
    this._region = region;
  }
  async me($ctx: UserContext): Promise<M.User> {
    const users = await this._store.usersFind($ctx);
    if (users.length == 0) {
      //if (process.env.NODE_ENV == 'local') {
      const fields = {};
      const result = await this._store.usersCreate($ctx, $ctx.sub, fields, this._locale, this._region);
      return result;
      //} else {
      //throw new Error('user not found');
      //}
    }
    return users[0];
  }

  async updateMe($ctx: UserContext, fields: M.UserFields): Promise<M.User> {
    const users = await this._store.usersFind($ctx);
    if (users.length == 0) {
      //if (process.env.NODE_ENV == 'local') {
      const result = await this._store.usersCreate($ctx, $ctx.sub, fields, this._locale, this._region);
      return result;
      //} else {
      //throw new Error('failed to update user');
      //}
    }

    const updates = await this._store.usersUpdate($ctx, users[0].id, fields);
    if (updates == undefined) throw new Error('failed to update user');
    return updates;
  }
}


