import * as React from 'react';
import { Root } from '../hooks/useroot';
import { TryAgain } from './tryagain';
import * as UI from '@inf/core-ui';
import { createCanceler } from '../tools/canceler';
import { ApolloError, ApolloQueryResult, ObservableQuery } from 'apollo-client';
import { AsyncStorage } from 'react-native';
import { LoadResult } from '../front/mobileapp/src/components/hocs/models';
import { createGetter, Getter } from '../tools/creategetter';
import { print } from 'graphql/language/printer';
import { WakeableTimeout } from '../hooks/useretrywaker';

const isNotEmptyObj = <T extends {}>(t: T | {}): t is T => {
  return Object.keys(t).length > 0;
}

type Mode = 'READY' | 'LOADING' | 'FETCHING_MORE' | 'REFETCHING';

const md5 = require('md5');

type Props<R> = {
  count?: number,
  onLoad: () => Promise<LoadResult<R>>,
  children: (data: R, opts: {
    fetchMore?: () => Promise<void>,
    refetch: () => Promise<void>,
    busy: boolean,
  }) => JSX.Element | null,
  expiresInMinutes?: number,
};

type State<R> = {
  mode: Mode,
};

class Loader<R> extends React.Component<Props<R>, State<R>> {
  private _c = createCanceler();
  //private _subscription?: Subscription;
  private _timer?: NodeJS.Timer;
  private _wakeableTimeout?: WakeableTimeout;
  private _query?: ObservableQuery<R>;
  private _data: Getter<LoadResult<R>>;

  constructor(props: Props<R>) {
    super(props);

    this._data = createGetter(this.props.onLoad());

    // detect if query has pagination
    this._data.promise().then(result => {
      let hasAfter = false;
      let hasCount = false;
      const sub = result.subscription;
      sub.options.query.definitions.forEach(def => {
        if (def.kind == 'OperationDefinition' && def.operation == 'query' && def.variableDefinitions != undefined) {
          def.variableDefinitions.forEach(vdef => {
            if (vdef.kind == 'VariableDefinition' && vdef.variable.kind == 'Variable' && vdef.type.kind == 'NamedType') {
              if (vdef.variable.name.value == 'after' && vdef.type.name.value == 'String') {
                hasAfter = true;
              } else if (vdef.variable.name.value == 'count' && vdef.type.name.value == 'Int') {
                hasCount = true;
              }
            }
          });
        }
      });
      const isPaginated = hasAfter && hasCount;

      // if paginated, initialize after and count variables
      this._query = sub;
      const variables = isPaginated ? { after: null, count: this.props.count || null } : undefined;
      if (variables != undefined) {
        sub.options.variables = {
          ...(sub.options.variables || {}),
          ...variables,
        };
      }
    }).catch(console.error);

    this.state = {
      mode: 'LOADING',
    };
  }

  private needsRefresh = async () => {
    if (this.props.expiresInMinutes == undefined) return false;

    const sub = this._query || (await this._data.promise()).subscription;
    this._query = sub;
    const hash = md5(`${print(sub.options.query)}.${JSON.stringify(sub.options.variables)}`);
    const key = `LOADER.CACHEKEY[${hash}.${hash.length}]`;

    const prevMSecs = Number.parseInt((await this._c.add(AsyncStorage.getItem(key)) || '0'));
    const nowMSecs = new Date().getTime();
    if (isNaN(prevMSecs)) {
      await AsyncStorage.setItem(key, nowMSecs.toString());
      return true;
    }

    const expired = (nowMSecs - prevMSecs) > (this.props.expiresInMinutes * 1000 * 60);
    if (!expired) return false;

    await AsyncStorage.setItem(key, nowMSecs.toString());
    return true;
  }

  private refetch = async () => {
    this.setState({
      mode: 'REFETCHING',
    });

    const sub = this._query = (await this._data.promise()).subscription;
    this._query = sub;
    sub.refetch({ count: this.props.count || null });
  }

  async componentDidMount() {
    Root.incrementRequestCount();
    const sub = this._query || (await this._data.promise()).subscription;
    this._query = sub;
    /*
    this._subscription = sub.subscribe({
      next: (value: ApolloQueryResult<R>) => {
        this._wakeableTimeout = undefined;
        Root.decrementRequestCount();
        this.setState({ mode: 'READY' });
      },
      error: (err) => {
        if (err.message == 'Network error: Network request failed') {
          if (this._wakeableTimeout == undefined)
            this._wakeableTimeout = new WakeableTimeout('loader');
          this._wakeableTimeout.wake(async () => sub.refetch());
        } else {
          Root.decrementRequestCount();
          this.setState({ mode: 'READY' });
        }
      },
      complete: () => {
        console.warn('loader complete');
      },
    });
    */
  }

  componentWillUnmount() {
    //if (this._subscription != undefined) {
    //this._subscription.unsubscribe();
    //this._subscription = undefined;
    //}
    if (this._timer != undefined) {
      clearTimeout(this._timer);
    }
  }

  async componentDidUpdate(prevProps: Props<R>, prevState: State<R>) {
    if (prevState.mode != this.state.mode && this.state.mode == 'READY' && await this.needsRefresh()) {
      await this.refetch();
    }
  }

  fetchMore = async () => {
    const data = await this._data.promise();
    if (data.fetchMore != undefined) {
      this.setState({ mode: 'FETCHING_MORE' });
      await data.fetchMore(this.props.count == undefined ? undefined : { count: this.props.count });
    }
  }

  hasMore = () => {
    if (this._data.current != undefined) {
      const result = this._data.current();
      if (result != undefined && result.hasMore != undefined)
        return result.hasMore();
    }
    return false;
  }

  render() {
    const result = this._getQueryResult(this._data.current());
    if (result.data == undefined) {
      return <UI.Loading />;
    } else if (result.error != undefined) {
      return <TryAgain error={result.error} onRetry={this.refetch} />;
    } else {
      const fetchMore = this.hasMore() ? this.fetchMore : undefined;
      return this.props.children(result.data, {
        fetchMore,
        refetch: this.refetch,
        busy: this.state.mode != 'READY',
      });
    }
  }

  _getQueryResult = <R extends {}>(result: LoadResult<R> | undefined): { loading: boolean, data?: R, error?: ApolloError } => {
    if (result == undefined) {
      return { loading: true };
    } else {
      const { data, loading, error } = result.subscription.currentResult();
      if (isNotEmptyObj(data)) {
        return { loading, error, data };
      } else {
        return { loading, error };
      }
    }
  }
}

export default Loader;
