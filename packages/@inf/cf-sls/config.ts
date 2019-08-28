import { createConfigRecord } from '@inf/vars/configure';
import { entries } from '@inf/common';
import * as fs from 'fs';
import * as yamljs from 'yamljs';
import * as RT from 'runtypes';

const SLSConfigRecord = RT.Record({
  service: RT.String,
  provider: RT.Record({
    name: RT.String,
    runtime: RT.String,
    stage: RT.String,
    region: RT.String,
  }).And(RT.Partial({
    vpc: RT.Record({
      securityGroupIds: RT.Array(RT.String),
      subnetIds: RT.Array(RT.String)
    }),
    iamRoleStatements: RT.Array(RT.Record({
      Effect: RT.String,
      Resource: RT.Array(RT.String),
      Action: RT.Array(RT.String)
    })),
  })),
}).And(RT.Partial({
  package: RT.Record({
    include: RT.Array(RT.String).Or(RT.Undefined),
    exclude: RT.Array(RT.String).Or(RT.Undefined)
  }),
  functions: RT.Dictionary(RT.Record({
    handler: RT.String,
  }).And(RT.Partial({
    environment: RT.Dictionary(RT.String),
    events: RT.Array(RT.Record({
      http: RT.Record({
        authorizer: RT.String,
        path: RT.String,
        method: RT.String,
        cors: RT.Boolean,
      })
    }))
  }))),
  plugins: RT.Array(RT.String),
}));

const DockerRecord = RT.Record({
  version: RT.String,
  services: RT.Dictionary(RT.Record({
    container_name: RT.String,
    ports: RT.Array(RT.String),
    image: RT.String,
  }).And(RT.Partial({
    environment: RT.Array(RT.String)
  }))),
});

type Handlers = { [_: string]: { filename: string, entrypoint: string } };

type SLSConfig = RT.Static<typeof SLSConfigRecord>;

type SLSVpc = SLSConfig['provider']['vpc'];

type SLSIamRoleStatement = SLSConfig['provider']['iamRoleStatements'][0];

type SLSFunction = SLSConfig['functions'][''];

type DockerService = RT.Static<typeof DockerRecord>['services'][''];

export type InputsAux = {
  name: string,
  alias?: string,

  rootDir: string,
  handlers: Handlers,

  stage: string,
  region: string,
  slsVpc?: SLSVpc,
  slsIamRoleStatements?: SLSIamRoleStatement[]
  slsIncludes?: string[],
  slsExcludes?: string[],
  slsFunctions?: { [_: string]: SLSFunction },
  slsPlugins?: string[],

  webpackIgnore?: RegExp,

  dockerServices?: { [_: string]: DockerService }
};

const ConfigAux = (inputs: InputsAux) => createConfigRecord(({ configurationDir }) => {

  if (!fs.existsSync(`${__dirname}/tmp`))
    fs.mkdirSync(`${__dirname}/tmp`);
  const instdir = `${__dirname}/tmp/${inputs.name}`;
  if (!fs.existsSync(instdir))
    fs.mkdirSync(instdir);

  // generate serverless.yml
  const sls = {
    service: inputs.alias || inputs.name,
    provider: {
      name: 'aws',
      runtime: 'nodejs10.x',
      stage: inputs.stage,
      region: inputs.region,
      vpc: inputs.slsVpc,
      iamRoleStatements: inputs.slsIamRoleStatements,
    },
    package: undefined,
    functions: inputs.slsFunctions,
    plugins: inputs.slsPlugins,
  };
  sls.provider.stage = inputs.stage;
  sls.provider.region = inputs.region;
  if (inputs.slsVpc) sls.provider.vpc = inputs.slsVpc;
  if (inputs.slsIamRoleStatements) sls.provider.iamRoleStatements = inputs.slsIamRoleStatements;
  if ((inputs.slsIncludes || inputs.slsExcludes) && !sls.package) sls.package = { include: undefined, exclude: undefined };
  if (inputs.slsIncludes) sls.package.include = inputs.slsIncludes;
  if (inputs.slsExcludes) sls.package.exclude = inputs.slsExcludes;
  if (inputs.slsFunctions) sls.functions = inputs.slsFunctions;
  if (!SLSConfigRecord.guard(sls))
    throw new Error('invalid sls configuration');

  // generate webpack.config.js
  const webpackconf = `// @ts-check
// this file has been automatically generated

const path = require('path');
const webpack = require('webpack');
module.exports = {
  mode: 'production',
  entry: { ${entries(inputs.handlers).map(([k, v]) => `${k}: path.resolve('./build/src/${v.filename.replace(/(.ts$)/, '.js')}')`).join(', ')} },
  target: 'node',
  output: {
    path: '${instdir}/build',
    filename: '_[name].js',
    libraryTarget: 'this'
  },
  ${inputs.webpackIgnore ? `plugins: [new webpack.IgnorePlugin(/${inputs.webpackIgnore.source}/)],` : ''}
  module: {},
  optimization: {
    minimize: process.env.NODE_ENV == 'production',
    concatenateModules: true,
  }
};`;

  // generate tsconfig.sls.json
  const tsconfig = {
    extends: `${configurationDir}/tsconfig-base`,
    compilerOptions: {
      composite: true,
      outDir: './build',
      rootDir: '.',
      noEmit: false,
      inlineSourceMap: true,
      inlineSources: true,
      noImplicitUseStrict: false,
      alwaysStrict: true,
      module: 'commonjs',
      target: 'es6',
      lib: [
        'dom',
        'es5',
        'scripthost',
        'es2017',
        'esnext.asynciterable'
      ],
      baseUrl: './src',
      typeRoots: [
        './node_modules/@types',
        './src/api/@types'
      ],
      types: [
        'node',
        'jest'
      ]
    },
    include: [
      './src/**/*',
      ...entries(inputs.handlers).map(([k, v]) => v.filename)
    ],
    exclude: [
      'build',
      'node_modules'
    ]
  };

  // generate package.json
  const packagejson = {
    "name": "@inf/cf-sls.cf-sls",
    "version": "0.0.1",
    "main": "index.js",
    "license": "MIT",
    "private": true,
    "scripts": {
      "concur": "concurrently",
      "gqlgen": "graphql-codegen",
      "prisma": "prisma",
      "sls": "serverless",
      "tsc": "../node_modules/typescript/bin/tsc",
      "webpack": "webpack",
      "test": "jest"
    },
    "dependencies": {
      "@inf/cf-gen": "*",
      "aws-lambda": "^0.1.2",
      "graphql": "14.4.2",
      "graphql-tools": "^4.0.5"
    },
    "devDependencies": {
      "@inf/cf-cognito": "*",
      "@inf/vars": "*",
      "@types/aws-lambda": "^8.10.31",
      "@types/graphql": "^14.2.3",
      "@types/jest": "^24.0.18",
      "@types/node": "^12.6.8",
      "@types/yamljs": "^0.2.30",
      "babel-jest": "^24.8.0",
      "jest": "^24.8.0",
      "jwk-to-pem": "^2.0.1",
      "serverless": "^1.48.4",
      "serverless-offline": "^5.10.1",
      "ts-jest": "^24.0.2",
      "webpack": "4.28.4",
      "webpack-cli": "3.3.6",
      "yamljs": "^0.3.0"
    }
  }

  // generate jest config
  const jestconfig = `module.exports = {
  preset: 'ts-jest',
  testEnvironment: 'node',
  transform: {
    '^.+\\.tsx?$': 'ts-jest',
  },
  testPathIgnorePatterns: ["/build/"]
};
`;

  // generate makefile
  const makefile = `SRC=../../../api/src
test:
	yarn gen wipetest
	yarn vars yarn test

start:
	docker-compose up -d && \
		yarn vars yarn serverless offline --host 0.0.0.0 --stage {{STAGE}} --skipCacheInvalidation true

invoke:
	@yarn vars curl -H 'Authorization: Bearer Poopy' -H 'Content-Type: application/json' -d "hello world" {{CF_API_GraphQLEndpoint}}
invoke.guest:
	@yarn vars curl -H 'Authorization: Guest' -H 'content-type: application/json' -d '{"query":"query($$arg: String!) { testUnauthorized (arg: $$arg) }","variables": { "arg": "pong" }}' {{CF_API_GraphQLEndpoint}}

logs.auth:
	yarn vars sls logs -s {{STAGE}} -f auth -t -r {{AWS_REGION}}
logs.api:
	yarn vars sls logs -s {{STAGE}} -f api -t -r {{AWS_REGION}}

build:
ifndef ROOTDIR
	$(error ROOTDIR is undefined)
endif
ifndef INSTDIR
	$(error INSTDIR is undefined)
endif
	ln -sf $(ROOTDIR) $(INSTDIR) && \
		cd $(INSTDIR) && \
		rm -f build/tsconfig.tsbuildinfo && tsc -b tsconfig.sls.json && yarn webpack --config $(INSTDIR)/webpack.config.js && cp build/_api.js build/api.js && cp build/_auth.js build/auth.js

deploy: build
	yarn vars yarn sls deploy --no-confirm

.PHONY: build deploy
`;

  const dockerServices = inputs.dockerServices && { version: '2', services: inputs.dockerServices } || undefined;
  if (dockerServices && !DockerRecord.guard(dockerServices))
    throw new Error('invalid docker config');

  entries(inputs.handlers).forEach(([k, v]) => {
    fs.writeFileSync(`${instdir}/${v.filename}`, `// this file has been automatically generated

// handler forwarding needed for serverless offline
// @ts-ignore
export { ${v.entrypoint} } from './src/${v.filename.replace(/\.[^/.]+$/, '')}';`, { encoding: 'utf8' });
  });
  fs.writeFileSync(`${instdir}/serverless.yml`, yamljs.stringify(sls, 1000, 2), { encoding: 'utf8' });
  fs.writeFileSync(`${instdir}/webpack.config.js`, webpackconf, { encoding: 'utf8' });
  fs.writeFileSync(`${instdir}/tsconfig.sls.json`, JSON.stringify(tsconfig, null, 2), { encoding: 'utf8' });
  fs.writeFileSync(`${instdir}/package.json`, JSON.stringify(packagejson, null, 2), { encoding: 'utf8' });
  fs.writeFileSync(`${instdir}/jest.config.js`, jestconfig, { encoding: 'utf8' });
  fs.writeFileSync(`${instdir}/Makefile`, makefile, { encoding: 'utf8' });
  if (!dockerServices) {
    if (fs.existsSync(`${instdir}/docker-compose.yml`))
      fs.unlinkSync(`${instdir}/docker-compose.yml`);
  } else {
    fs.writeFileSync(`${instdir}/docker-compose.yml`, yamljs.stringify(dockerServices, 1000, 2), { encoding: 'utf8' });
  }

  return {
    type: 'shell',
    name: inputs.name,
    env: {
      ROOTDIR: `${configurationDir}/${inputs.rootDir}`,
      INSTDIR: `${instdir}`
    },
    cwd: instdir,
    command: 'make',
    args: inputs.stage == 'local' ? ['build'] : ['deploy'],
  };
});

type Inputs = {
  rootDir: string,
  handlers: Handlers,

  stage: string,
  region: string,
  accountId: string,
  cognitoUserPoolId: string,
  securityGroupIds: string[],
  subnetIds: string[],
}

export const Config = (inputs: Inputs) => ConfigAux({
  name: 'cf-sls',
  alias: 'api',

  rootDir: inputs.rootDir,
  handlers: inputs.handlers,

  stage: inputs.stage,
  region: inputs.region,
  slsVpc: {
    securityGroupIds: inputs.securityGroupIds,
    subnetIds: inputs.subnetIds,
  },
  slsIamRoleStatements: [{
    Effect: 'Allow',
    Resource: [`arn:aws:cognito-idp:${inputs.region}:${inputs.accountId}:userpool/${inputs.cognitoUserPoolId}`],
    Action: [
      'cognito-idp:AdminCreateUser',
      'cognito-idp:AdminDeleteUser',
      'cognito-idp:AdminConfirmSignUp',
      'cognito-idp:AdminAddUserToGroup',
      'cognito-idp:AdminDisableUser',
      'cognito-idp:AdminEnableUser',
      'cognito-idp:ListUsers',
      'cognito-idp:AdminUpdateUserAttributes',
      'cognito-idp:AdminGetUser'
    ]
  }, {
    Effect: 'Allow',
    Action: [
      's3:ListBucket',
      's3:PutObject',
      's3:PutObjectAcl',
      's3:AbortMultipartUpload',
      's3:GetObject',
      's3:GetObjectAcl',
      's3:ListBucketMultipartUploads',
      's3:ListMultipartUploadParts',
      's3:AbortMultipartUpload'
    ],
    Resource: [
      `arn:aws:s3:::uploads-${inputs.stage}-${inputs.region}`,
      `arn:aws:s3:::uploads-${inputs.stage}-${inputs.region}/*`
    ]
  }],
  slsIncludes: ['build/api.js', 'build/auth.js'],
  slsExcludes: ['**/*'],
  slsFunctions: {
    auth: {
      handler: './build/auth.handler'
    },
    api: {
      handler: './build/api.handler',
      environment: {
        STAGE: inputs.stage,
      },
      events: [{
        http: {
          authorizer: 'auth',
          path: 'graphql',
          method: 'post',
          cors: true
        }
      }]
    }
  },
  slsPlugins: ['serverless-offline'],
  webpackIgnore: /^pg-native$/,
  dockerServices: {
    redis_local: {
      container_name: 'redis_local',
      ports: ['6379:6379'],
      image: 'redis:3.2.10-alpine'
    },
    postgres_local: {
      container_name: 'postgres_local',
      ports: ['5432:5432'],
      image: 'mdillon/postgis:9.6-alpine',
      environment: [
        'POSTGRES_USER=root',
        'POSTGRES_PASSWORD=for-development-only',
        'POSTGRES_DB=mainlocal'
      ]
    }
  }
})