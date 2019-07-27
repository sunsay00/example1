import { domainWrapper } from 'core';
import { APIGatewayProxyEvent, Context } from 'aws-lambda';
import { createConfig } from 'common';

const config = createConfig({
  STAGE: process.env.STAGE
});

export const handler = domainWrapper(async (event: APIGatewayProxyEvent, context: Context) => {
  return {
    statusCode: 200,
    body: JSON.stringify({}),
  };
})
