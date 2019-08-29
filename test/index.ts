import { domainWrapper } from '@inf/core';
import { APIGatewayProxyEvent, Context } from 'aws-lambda';

export const handler = domainWrapper(async (event: APIGatewayProxyEvent, context: Context) => {
  return {
    hello: 'world'
  };
});
