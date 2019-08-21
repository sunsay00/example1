import { adminEnableDisableUser } from '../tools/awsclient';
import { IStore, Point, Cursorize, Dict, ISellersPartialService } from '@inf/cf-gen';
import * as M from '@inf/cf-gen/model';
import { UserContext } from '../types';
import NotificationManager from '../tools/notificationmanager';

export default class SellersService implements ISellersPartialService<UserContext> {
  private _store: IStore<UserContext>;

  constructor(_notifications: NotificationManager, store: IStore<UserContext>) {
    this._store = store;
  }

  search = async ($ctx: UserContext, __fields: string[], location: Point, radiusMiles: number, make?: string, after?: string, count?: number): Promise<(M.Seller & Cursorize)[]> => {
    const dealerships = await this._store.dealershipsSearch($ctx, location, radiusMiles, make, undefined, 100);
    if (dealerships.length == 0) return [];
    const dealershipMap = {} as Dict<M.Dealership>;
    dealerships.forEach(d => dealershipMap[d.id] = d);
    return (await this._store.sellersFindByDealershipIdIn($ctx, dealerships.map(d => d.id), after, count)).map(s => {
      const { dealershipId, ...rest } = s;
      const seller = { ...rest } as M.Seller & Cursorize;
      seller.dealership = dealershipMap[dealershipId];
      return seller;
    }).sort((a, b) => a.id.localeCompare(b.id)); // let's sort it by id to stabilize the result set
  }

  searchByDistance = async ($ctx: UserContext, __fields: string[], location: Point, make?: string, after?: string, count?: number): Promise<(M.Seller & Cursorize)[]> => {
    const dealerships = await this._store.dealershipsSearchByDistance($ctx, location, make, undefined, 100);
    if (dealerships.length == 0) return [];
    const dealershipMap = {} as Dict<M.Dealership & M.Distanced>;
    dealerships.forEach(d => dealershipMap[d.id] = d);
    const ret = [] as (M.Seller & M.Distanced & Cursorize)[];
    (await this._store.sellersFindNonDisabledByDealershipIdIn($ctx, dealerships.map(d => d.id), after, count)).map(s => {
      const { dealershipId, ...rest } = s;
      const seller = { ...rest } as M.Seller & Cursorize;
      const dealership = dealershipMap[dealershipId];
      seller.dealership = dealership;
      if (dealership != undefined && dealership.distance != undefined) {
        ret.push({ ...seller, distance: dealership == undefined ? 0 : dealership.distance });
      }
    });
    return ret.sort((a, b) => a.distance - b.distance); // let's sort it by distance to stablize the result set
  }

  setDisabled = async ($ctx: UserContext, id: string, dealershipId: string, disabled: boolean): Promise<M.Seller | undefined> => {
    try {
      this._store.beginTransaction();
      const ret = await this._store.sellersSetDisabled($ctx, id, dealershipId, disabled);
      if (ret != undefined) {
        await adminEnableDisableUser(ret.sub, dealershipId, disabled);
        this._store.commitTransaction();
      } else {
        this._store.rollbackTransaction();
        return ret;
      }
      return ret;
    } catch (ex) {
      this._store.rollbackTransaction();
      throw ex;
    }
  }

}
