import { grant } from './grant';
import { IUserContext } from '../types';

class TestingService {

  @grant(['admins'])
  privateUpperCase(user: IUserContext, arg: string): string { return arg.toUpperCase(); }

  @grant([])
  protectedUpperCase(user: IUserContext, arg: string): string { return arg.toUpperCase(); }

  publicUpperCase(user: IUserContext, arg: string): string { return arg.toUpperCase(); }
};

export default TestingService;
