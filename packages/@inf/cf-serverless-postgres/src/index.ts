import { Client, QueryResult } from 'pg';
import * as utils from 'pg/lib/utils';

export type QueryResult = {
  command: string;
  rowCount: number;
  oid: number;
  rows: any[];
}
export type QueryParam = object | number | string | undefined;

export type IRDSDBClient = {
  query(query: string, params: QueryParam[]): Promise<QueryResult>;
}

const sanitizeLiteral = (str: string): string => {
  let hasBackslash = false;
  let escaped = '\'';
  for (let i = 0; i < str.length; ++i) {
    const c = str[i];
    if (c === '\'') {
      escaped += c + c;
    } else if (c === '\\') {
      escaped += c + c;
      hasBackslash = true;
    } else {
      escaped += c;
    }
  }
  escaped += '\'';
  if (hasBackslash === true) {
    escaped = ' E' + escaped;
  }
  return escaped;
};

const sanitizeIdentifier = (str: string): string => {
  let escaped = '"';
  for (let i = 0; i < str.length; ++i) {
    let c = str[i]
    if (c === '"') {
      escaped += c + c;
    } else {
      escaped += c;
    }
  }
  escaped += '"';
  return escaped;
};

//onUnload(async () => uninit());

export default class RDSDBClient implements IRDSDBClient {
  private _rdsDbEndpoint: string;
  private _client: Client | undefined = undefined;

  constructor(rdsDbEndpoint: string) {
    this._rdsDbEndpoint = rdsDbEndpoint;
  }

  init = async () => {
    const matches = /postgres:\/\/([^:]+):([^@]+)@([^:^\/]+):?([0-9]+)?\/(.+$)/.exec(this._rdsDbEndpoint);
    if (!matches) throw new Error('failed to parse AWS_RDS_DB_ENDPOINT');
    if (matches[4] !== undefined && isNaN(parseInt(matches[4]))) throw new Error(`invalid RDS port of '${matches[4]}'`);
    const connectParams = {
      user: matches[1],
      password: matches[2],
      host: matches[3],
      port: parseInt(matches[4] || '5432'),
      database: matches[5],
      //max: 10,
      //idleTimeoutMillis: 30000,
    };
    const client = new Client(connectParams);
    this._client = client;
    await new Promise<void>((resolve, reject) => client.connect(err => err ? reject(err) : resolve()));
  };

  reset = async () => {
    if (process.env.NODE_ENV != 'test') {
      throw new Error('ERROR!! database reset should only be called in the test environment! ignoring');
    }
    if (this._client == undefined) throw new Error(`RDS client is undefined ${this._rdsDbEndpoint}`);
    const client = this._client;
    return new Promise((resolve, reject) =>
      client.query('DELETE FROM users', [], (err, result) => err ? reject(err) : resolve())
    );
  }

  deinit = async () => {
    if (this._client != undefined) {
      const client = this._client;
      await new Promise<void>((resolve, reject) => client.end(err => err ? reject(err) : resolve()));
    }
  };

  async beginTransaction() {
    if (this._client == undefined) throw new Error(`RDS client is undefined ${this._rdsDbEndpoint}`);
    const client = this._client;
    return client.query('BEGIN');
  }
  async rollbackTransaction() {
    if (this._client == undefined) throw new Error(`RDS client is undefined ${this._rdsDbEndpoint}`);
    const client = this._client;
    return client.query('ROLLBACK');
  }
  async commitTransaction() {
    if (this._client == undefined) throw new Error(`RDS client is undefined ${this._rdsDbEndpoint}`);
    const client = this._client;
    return client.query('COMMIT');
  }
  prepareString = (value: unknown): string => {
    if (typeof value == 'number') {
      return utils.prepareValue(value);
    } else {
      return sanitizeLiteral(utils.prepareValue(value));
    }
  }
  static prepareIdentifier = (value: string): string => {
    return sanitizeIdentifier(value);
  }
  async query(query: string, params: (object | number | string | undefined)[]) {
    if (this._client == undefined) throw new Error(`RDS client is undefined ${this._rdsDbEndpoint}`);
    const client = this._client;
    return new Promise<QueryResult>((resolve, reject) => {
      client.query(query, params, (err, result) => {
        if (err) reject(err);
        else resolve(result);
      });
    });
  };
}
