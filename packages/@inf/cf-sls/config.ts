import { createConfig, ShellOutputMatchers, writeTmps } from '@inf/vars/configure';
import { fromEntries, entries, Diff, capitalizeFirstLetter } from '@inf/common';
import { unlinkRecursiveSync } from '@inf/core';

import * as fs from 'fs';
import * as path from 'path';
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

type SLSConfig = RT.Static<typeof SLSConfigRecord>;
type SLSVpc = SLSConfig['provider']['vpc'];
type SLSIamRoleStatement = Diff<SLSConfig['provider']['iamRoleStatements'], undefined>[0];
type SLSFunction = Diff<SLSConfig['functions'], undefined>[''];
type DockerService = RT.Static<typeof DockerRecord>['services'][''];

type StartCommand = {
  command: string,
  args: string[],
};

export type Handler = {
  vars?: { [_: string]: string },
  packageJsonPath: string,
  filepath: string,
  entrypoint: string
  environment?: SLSFunction['environment'],
  events?: SLSFunction['events']
};

const PackageJsonRecord = RT.Partial({
  dependencies: RT.Dictionary(RT.String),
  devDependencies: RT.Dictionary(RT.String)
});

export const Config = (inputs: {
  id: string,
  alias?: string,

  rootDir: string,

  dependsOn?: string[],

  vars?: { [_: string]: string },

  handlers: { [_: string]: Handler },

  slsVpc?: SLSVpc,
  slsIamRoleStatements?: SLSIamRoleStatement[]
  slsIncludes?: string[],
  slsExcludes?: string[],
  slsPlugins?: string[],

  webpackIgnore?: RegExp,

  dockerServices?: { [_: string]: DockerService },

  packageDependencies?: { [_: string]: string },
  packageDevDependencies?: { [_: string]: string },

  startCommands?: StartCommand[]
}) => createConfig(({ stage, region, configurationDir }) => ({
  clean: async () => {
    if (fs.existsSync(`${__dirname}/tmp`))
      unlinkRecursiveSync(`${__dirname}/tmp`);
    return {};
  },
  up: async () => {
    if (!fs.existsSync(`${__dirname}/tmp`))
      fs.mkdirSync(`${__dirname}/tmp`);
    const instdir = `${__dirname}/tmp/${inputs.id}`;
    if (!fs.existsSync(instdir))
      fs.mkdirSync(instdir);

    entries(inputs.handlers).forEach(([_, v]) => {
      if (v.vars) {
        const tsdir = path.dirname(v.packageJsonPath)
        const key = inputs.id.replace(/-/g, '_').toUpperCase();
        writeTmps('vars', tsdir, v.vars);
      }
    });

    const { dependencies, devDependencies } = entries(inputs.handlers).reduce((acc, [_, v]) => {
      try {
        const packagejson = JSON.parse(fs.readFileSync(v.packageJsonPath, { encoding: 'utf8' }));
        if (!PackageJsonRecord.guard(packagejson)) {
          return { dependencies: acc.dependencies, devDependencies: acc.devDependencies };
        } else {
          return {
            dependencies: { ...acc.dependencies, ...(packagejson.dependencies || {}) },
            devDependencies: { ...acc.devDependencies, ...(packagejson.devDependencies || {}) }
          };
        }
      } catch (err) {
        throw new Error('failed to parse package.json');
      }
    }, { dependencies: inputs.packageDependencies || {}, devDependencies: inputs.packageDevDependencies || {} });

    const maybePush = <T>(x: T | T[], r?: T[]): T[] =>
      Array.isArray(x) ? (r ? [...x, ...r] : x) : (r ? [x, ...r] : [x]);

    // generate serverless.yml
    const sls = {
      service: inputs.alias || inputs.id,
      provider: {
        name: 'aws',
        runtime: 'nodejs10.x',
        stage,
        region,
        vpc: inputs.slsVpc,
        iamRoleStatements: inputs.slsIamRoleStatements,
      },
      package: {
        include: maybePush(entries(inputs.handlers).map(([_, v]) => `build/${v.filepath.replace(/(.ts$)/, '.js')}`), inputs.slsIncludes),
        exclude: maybePush('**/*', inputs.slsExcludes)
      },
      functions: fromEntries(entries(inputs.handlers).map(([k, v]) => [k, {
        handler: `./build/${v.filepath.replace(/(.ts$)/, '')}.${v.entrypoint}`,
        environment: v.environment,
        events: v.events
      }])),
      plugins: ['serverless-offline', ...inputs.slsPlugins || []],
    };
    if (!SLSConfigRecord.guard(sls))
      throw new Error('invalid sls configuration');

    const dirnames = entries(inputs.handlers).map(([k, v]) => ({
      dirname: path.dirname(v.packageJsonPath),
      filename: v.filepath
    }));

    const computeCommonAncestorPath = (paths: string[]) => {
      if (paths.length == 0) return '';
      const splits = paths.map(p => p.split('/'));
      const n = splits.reduce((a, b) => a == 0 ? b.length : Math.min(a, b.length), 0);
      let ret = 0;
      for (let i = 0; i < n; ++i) {
        for (let j = 1; j < splits.length; ++j)
          if (splits[0][i] != splits[j][i])
            return splits[0].slice(0, ret).join('/');
        ++ret;
      }
      return splits[0].slice(0, n).join('/');
    }

    const absRoots = dirnames.map(v => ({
      abspath: path.resolve(v.dirname.startsWith('/') ? v.dirname : `${configurationDir}/${v.dirname}`),
      filename: v.filename.replace(/(.ts$)/, '.js'),
    }));
    const commonAncestor = computeCommonAncestorPath(absRoots.map(v => v.abspath));

    const rootDirs = dirnames.map(v => {
      const ROOTDIR = v.dirname.startsWith('/') ? v.dirname : `${configurationDir}/${v.dirname}/`;
      return path.relative(instdir, ROOTDIR);
    });

    const rootDir = rootDirs.length == 1 ? rootDirs[0] : '../'.repeat(rootDirs.map(r => {
      const m = r.match(/^(\.\.\/)+/g);
      return !m || m.length != 1 ? 0 : m[0].length / 3;
    }).reduce((a, b) => a == 0 ? b : Math.max(a, b), 0));;

    // generate webpack.config.js
    const webpackconf = `// @ts-check
// this file has been automatically generated

const webpack = require('webpack');
module.exports = {
  mode: 'production',
  entry: { ${absRoots.map(v => {
      const p = v.abspath.replace(new RegExp(`^${commonAncestor}`), './build');
      return `${path.basename(v.filename.replace(/(.js$)/, ''))}: '${p}/${v.filename}'`;
    }).join(', ')} },
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
        rootDir,
        rootDirs,
        noEmit: false,
        inlineSourceMap: true,
        inlineSources: true,
        noImplicitUseStrict: false,
        alwaysStrict: true,
        module: 'commonjs',
        target: 'es6',
        lib: ['dom', 'es5', 'scripthost', 'es2017', 'es2018', 'es2019', 'esnext.asynciterable'],
        baseUrl: './src',
        typeRoots: ['./node_modules/@types'],
        types: ['node', 'jest']
      },
      include: [
        ...rootDirs.map(invReldir => [
          `${invReldir}/**/*.ts`
        ]).flat(),
      ],
      exclude: [
        'build',
        'node_modules',
        ...rootDirs.map(invReldir => [
          `${invReldir}/__tests__/**/*.ts`
        ]).flat(),
      ]
    };

    // generate package.json
    const packagejson = {
      "name": `@inf/cf-sls.${inputs.id}`,
      "version": "0.0.1",
      "main": "index.js",
      "license": "MIT",
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
        //"@inf/cf-gen": "*",
        //"aws-lambda": "0.1.2",
        //"graphql": "14.4.2",
        //"graphql-tools": "4.0.5",
        "@inf/vars": "*",
        ...(dependencies || {}),
      },
      "devDependencies": {
        //"@inf/cf-cognito": "*",
        //"@types/aws-lambda": "8.10.31",
        //"@types/graphql": "14.2.3",
        "@types/jest": "24.0.18",
        //"@types/node": "12.6.8",
        "babel-jest": "24.8.0",
        "jest": "24.8.0",
        "jwk-to-pem": "2.0.1",
        "serverless": "1.48.4",
        "ts-jest": "24.0.2",
        "webpack": "4.28.4",
        "webpack-cli": "3.3.6",
        "concurrently": "4.1.2",
        "nodemon": "1.19.2",
        "serverless-offline": "5.10.1",
        ...(devDependencies || {}),
      }
    }

    // generate jest config
    const jestconfig = `module.exports = {
  preset: 'ts-jest',
  testEnvironment: 'node',
  transform: {
    '^.+\\.tsx?$': 'ts-jest',
  },
  testPathIgnorePatterns: ["/build/"],
  roots: [
    ${dirnames.map(v => {
      const ROOTDIR = v.dirname.startsWith('/') ? v.dirname : `${configurationDir}/${v.dirname}/`;
      return `'<rootDir>/${path.relative(instdir, ROOTDIR)}'`;
    }).join(',\n    ')}
  ]
};
`;

    //start:
    //@$(YARN) vars $(YARN) sls offline
    //cf-sls-test-local-test 

    // generate makefile
    const buildInvokeRules = (handlers: { [_: string]: Handler }) =>
      entries(handlers).map<string[]>(([k, v]) =>
        v.events ? v.events.map(e =>
          `# DESC: invokes api endpoint
invoke.${k}:
\t@$(YARN) vars curl -H 'Authorization: Bearer FIXME' -H 'Content-Type: application/json' -d "hello world" {{${inputs.id.toUpperCase().replace(/-/g, '_')}_${capitalizeFirstLetter(k)}Endpoint}}
# DESC: invoke guest api endpoint
invoke.${k}.guest:
\t@$(YARN) vars curl -H 'Authorization: Guest' -H 'content-type: application/json' -d '{"query":"query($$arg: String!) { testUnauthorized (arg: $$arg) }","variables": { "arg": "pong" }}' {{${inputs.id.toUpperCase().replace(/-/g, '_')}_${capitalizeFirstLetter(k)}Endpoint}}`) :
          [`# DESC: invokes lambda function
invoke.${k}:
${stage == 'local' ?
              `\t@../../bin/invokelocallambda us-east-1 ${inputs.id}-local-${k} "$(SLS_SS_ARGS)"` :
              `\t@$(YARN) vars $(YARN) sls invoke --function ${k}`}`]
      ).flat().join('\n');

    const startCmds: StartCommand[] = [];
    if (inputs.dockerServices)
      startCmds.push({ command: 'docker-compose', args: ['up', '-d'] });
    if (inputs.startCommands)
      inputs.startCommands.forEach(c => startCmds.push(c));

    if (startCmds.length == 0)
      startCmds.push({ command: 'yarn', args: ['-s', 'concurrently', '--kill-others', `"nodemon --watch ./build --exec 'yarn -s sls offline'"`, '"tsc -w -p tsconfig.sls.json"'] });

    const makefile = `YARN = yarn -s

# DESC: run tests
test:
\t@$(YARN) gen wipetest && \\
\t\t$(YARN) vars $(YARN) test $(SLS_SS_ARGS)

${inputs.dockerServices && `# DESC: start docker
up:
\t@docker-compose up -d` || ''}

${inputs.dockerServices && `# DESC: stop docker
down:
\t@docker-compose down` || ''}

${startCmds.length == 0 ? '' : `# DESC: starts server
start:
${`\t@${startCmds.map(c => `$(YARN) vars ${c.command}${c.args.length == 0 ? '' : ` ${c.args.join(' ')}`}`).join(' && \\\n\t\t')}`}`}

${buildInvokeRules(inputs.handlers)}

${entries(inputs.handlers).map(([k, v]) => `# DESC: tails out log messages
logs.${k}:
\t@$(YARN) vars sls logs -s {{STAGE}} -f ${k} -t -r {{AWS_REGION}}`).join('\n')}

build:
\t@cd ${instdir} && \\
\t\t$(YARN) install --prefer-offline && \\
\t\trm -f build/tsconfig.tsbuildinfo && \\
\t\ttsc -p tsconfig.sls.json && \\
\t\t$(YARN) webpack --config ${instdir}/webpack.config.js && \\
\t\tmkdir -p build/src && \\
${entries(inputs.handlers).map(([k, v]) =>
      `\t\tcp build/_${path.basename(v.filepath.replace(/(.ts$)/, '.js'))} build/${v.filepath.replace(/(.ts$)/, '.js')}`).join(' && \\\n')}

deploy: build
\t@$(YARN) vars $(YARN) sls deploy --no-confirm

.PHONY: build deploy
`;

    fs.writeFileSync(`${instdir}/serverless.yml`, yamljs.stringify(sls, Number.MAX_SAFE_INTEGER, 2), { encoding: 'utf8' });
    fs.writeFileSync(`${instdir}/webpack.config.js`, webpackconf, { encoding: 'utf8' });
    fs.writeFileSync(`${instdir}/tsconfig.sls.json`, JSON.stringify(tsconfig, null, 2), { encoding: 'utf8' });
    fs.writeFileSync(`${instdir}/package.json`, JSON.stringify(packagejson, null, 2), { encoding: 'utf8' });
    fs.writeFileSync(`${instdir}/jest.config.js`, jestconfig, { encoding: 'utf8' });
    fs.writeFileSync(`${instdir}/Makefile`, makefile, { encoding: 'utf8' });
    if (inputs.dockerServices) {
      const dockerServices = inputs.dockerServices && { version: '2', services: inputs.dockerServices } || undefined;
      if (dockerServices && !DockerRecord.guard(dockerServices))
        throw new Error('invalid docker config');

      if (!dockerServices) {
        fs.existsSync(`${instdir}/docker-compose.yml`) && fs.unlinkSync(`${instdir}/docker-compose.yml`);
      } else {
        fs.writeFileSync(`${instdir}/docker-compose.yml`, yamljs.stringify(dockerServices, Number.MAX_SAFE_INTEGER, 2), { encoding: 'utf8' });
      }
    }

    //const dependsOn = [
    //...entries(inputs.handlers).map(([_, v]) => path.resolve(v.packageJsonPath)),
    //...entries(inputs.handlers).map(([_, v]) => `${path.resolve(path.dirname(v.packageJsonPath))}/**/*.ts`),
    //...(inputs.dependsOn || [])];
    const dependsOn = inputs.dependsOn;

    return {
      type: 'shell',
      rootDir: inputs.rootDir,
      dependsOn,
      vars: inputs.vars,
      id: inputs.id,
      cwd: instdir,
      command: 'make',
      args: stage == 'local' ? ['build'] : ['deploy'],
      outputMatchers: stage == 'local' ? undefined : buildOutputMatchers(inputs.handlers),
      outputs: buildDefaultReturns(stage, inputs.handlers)
    };
  }
}));

const buildDefaultReturns = (stage: string, handlers: { [_: string]: Handler }): { [_: string]: string } =>
  fromEntries(entries(handlers).filter(([k, v]) => !!v.events).map<[string, string][]>(([k, v]) =>
    v.events!.map(e => [`${capitalizeFirstLetter(k)}Endpoint`, stage == 'local' ? `http://0.0.0.0:3000/${e.http.path}` : ''])).flat());

const buildOutputMatchers = (handlers: { [_: string]: Handler }): ShellOutputMatchers => {
  const ents = entries(handlers);
  const fmapped = ents.map<[string, RegExp][]>(([k, v]) =>
    v.events ? v.events.map(_ =>
      [`${capitalizeFirstLetter(k)}Endpoint`, /Service Information[\s\S.]+endpoints:[\s\S.]+POST - (.+)$/gm]) :
      [[`${capitalizeFirstLetter(k)}Function`, new RegExp(`Service Information[\\s\\S]+functions:[\\s\\S]+${k}: (.+)`, 'gm')]]
  ).flat();
  return fromEntries(fmapped) as ShellOutputMatchers;
}
