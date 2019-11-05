//import Chat from '../chat';
import { inviteUser, adminDeleteUser, adminConfirmSignUp, adminAddUserToGroup, adminListUsers } from '../tools/awsclient';
import { Cursorize } from '@infng/cf-gen';
import { IStore, ISystemsService } from '../../_gen';
import * as M from '../../_gen';
import { UserContext } from '../types';
import NotificationManager from '../tools/notificationmanager';

export default class SystemsService implements ISystemsService<UserContext> {
  private _store: IStore<UserContext>;

  constructor(notifications: NotificationManager, store: IStore<UserContext>) {
    this._store = store;
  }

  /*
  postConfirm = async ($user: UserContext, locale: string, region: string): Promise<boolean> => {
    const result = await this._store.usersCreate($user, $user.sub, { name: $user.username, avatarUri: '' }, locale, region);

    // for integration testing, delete users with zero sub ids
    if ($user.sub == '00000000-0000-0000-00000000000000000') {
      const removeResult = await this._store.usersRemove($user, result.id);
      return !!removeResult;
    } else {
      return !!result;
    }
  }
  */

  getChatToken = async ($user: UserContext, __fields: string[], identity: string, pushChannel?: string): Promise<string> => {
    //return Chat.getToken(identity, pushChannel);
    return '';
  }

  inviteUser = async ($ctx: UserContext, newUsername: string, email: string, locale: string, dealershipId: string): Promise<boolean> => {
    return false;//await inviteUser(newUsername, email, locale, 'sales', dealershipId, undefined);//, 'RESEND'|'SUPRESS'|undefined);
  }

  inviteUserV2 = async ($ctx: UserContext, newUsername: string, email: string, locale: string, dealershipId: string, action?: string): Promise<M.PendingSeller | undefined> => {
    return await inviteUser(newUsername, email, locale, 'sales', dealershipId, action == 'RESEND' ? 'RESEND' : action == 'SUPRESS' ? 'SUPRESS' : undefined);
  }

  rateSeller = async ($ctx: UserContext, __fields: string[], sellerId: string, rating: number): Promise<number | undefined> => {
    if (rating <= 0 || rating > 4) return undefined;
    const seller = await this._store.sellersRate($ctx, sellerId, rating);
    if (seller == undefined || seller.numVotes <= 0) return undefined;
    return seller.rating / seller.numVotes;
  }

  executeTestCommand = async ($ctx: UserContext, command: string): Promise<string> => {
    if ($ctx.username == 'test-commander') { // only allow the e2e user to be deleted
      if (command == 'reset-e2e') {
        try {
          await adminDeleteUser('e2e-dealership');
          await adminDeleteUser('e2e-seller');
        } catch (err) {
          // it is alright if the user does not exist
          if (err.code != 'UserNotFoundException') {
            throw err;
          }
        }
        return 'success';
      } else if (command == 'confirm-dealership') {
        await adminConfirmSignUp('e2e-dealership');
        await adminAddUserToGroup('e2e-dealership', 'dealership');
        return 'success';
      } else if (command == 'reset-invite') {
        await adminDeleteUser('invite-tester');
        return 'success';
      }
    } else if ($ctx.username == 'tester' || $ctx.username == undefined) { // TODO: make username always defined
      if (command == 'run-smoketests') {
        return this._store.runSmokeTests();
      }
    }
    throw new Error(`invalid test command ${command} ${$ctx.username} ${$ctx.sub} [${($ctx.groups || []).join(', ')}]`);
  }
  getPendingSellers = async ($ctx: UserContext, __fields: string[], dealershipId: string, after?: string, count?: number): Promise<(M.PendingSeller & Cursorize)[]> => {
    const users = await adminListUsers('FORCE_CHANGE_PASSWORD', dealershipId, count, after);
    return users.map(m => ({
      sub: m.sub
      , username: m.username
      , email: m.email
      , name: m.name
      , cursor: m.token
    }));
  }
}
