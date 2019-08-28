import { IUserContext } from '../types';
import Mapper from '../back/api/src/api/mapper';

export type ITestingService<C extends IUserContext> = {
  adminAuthorized(user: IUserContext, fields: string[], mapper: Mapper<C>, arg: string): Promise<string>;
  authorized(user: IUserContext, fields: string[], mapper: Mapper<C>, arg: string): Promise<string>;
  unauthorized(user: IUserContext, fields: string[], mapper: Mapper<C>, arg: string): Promise<string>;
};