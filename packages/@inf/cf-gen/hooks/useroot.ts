import { WatchQueryOptions, ObservableQuery, MutationOptions, ApolloClient } from 'apollo-client';
import { FetchResult } from 'apollo-link';

import { keepTrying } from '../hooks/useretrywaker';

export const Root = ({
  mutate: <T>(options: MutationOptions<T>): Promise<FetchResult<T>> => {
    const _client: any = undefined;
    return keepTrying(() => _client!.mutate(options), 'Root.mutate');
  },
  watchQuery: <T>(options: WatchQueryOptions): Promise<ObservableQuery<T>> => {
    const _client: any = undefined;
    return keepTrying(() => Promise.resolve(_client!.watchQuery(options)), 'Root.watchQuery');
  },
  incrementRequestCount: () => {
    console.log('increment request count not yet implemented');
  },
  decrementRequestCount: () => {
    console.log('decrement request count not yet implemented');
  }
});