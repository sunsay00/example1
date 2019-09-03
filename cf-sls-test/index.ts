import { domainWrapper } from '@inf/core';
import { APIGatewayProxyEvent, Context } from 'aws-lambda';
import { vars } from './src/vars';
import { Client } from 'pg';

export const handler = domainWrapper(async (event: APIGatewayProxyEvent, context: Context) => {
  const matches = /postgres:\/\/([^:]+):([^@]+)@([^:^\/]+):?([0-9]+)?\/(.+$)/.exec(vars.DB_URL);
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
  await new Promise<void>((resolve, reject) => client.connect(err => err ? reject(err) : resolve()));
  if (!event.body) throw new Error('invalid body');

  const result = await client.query(event.body, []);
  const columns = ((result as any).fields.map((f: { name: string }) => f.name));

  const ret = `${columns.join('\t')}\n${result.rows.map((r, i) => r[columns[i]]).join('\t')}\n`;
  return ret;
});
