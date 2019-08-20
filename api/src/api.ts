import { domainWrapper } from '@inf/core';
import { APIGatewayProxyEvent, Context } from 'aws-lambda';
import { verifyVars } from '@inf/common';

const config = verifyVars({
  STAGE: process.env.STAGE
});

export const handler = domainWrapper(async (event: APIGatewayProxyEvent, context: Context) => {
  console.log('+++ 114');
  return {
    statusCode: 200,
    body: JSON.stringify({}),
  };
})
