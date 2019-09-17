import * as fs from 'fs';
import * as path from 'path';
import * as yamljs from 'yamljs';
import * as RT from 'runtypes';
import { useGlobals, useTempDir, useShell, useScriptRegistry, useDependsOn, useEffect, useCache } from '@inf/hookops';
import { vars } from '@inf/hookops/vars';
import { fromEntries, entries, Diff } from '@inf/common';
import { pathTransformer } from '@inf/core';
import { useVarsWriter } from '@inf/hooks';

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

type SLSConfig = RT.Static<typeof SLSConfigRecord>;
type SLSVpc = SLSConfig['provider']['vpc'];
type SLSIamRoleStatement = Diff<SLSConfig['provider']['iamRoleStatements'], undefined>[0];
type SLSFunction = Diff<SLSConfig['functions'], undefined>[''];

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

export const useLam = async <R>(props: {
  rootDir: string,

  id: string,
  alias?: string,

  dependsOn?: string[],

  vars?: { [_: string]: string },

  handlers: { [_ in keyof R]: Handler },

  slsVpc?: SLSVpc,
  slsIamRoleStatements?: SLSIamRoleStatement[]
  slsIncludes?: string[],
  slsExcludes?: string[],
  slsPlugins?: string[],

  webpackIgnore?: RegExp,

  packageDependencies?: { [_: string]: string },
  packageDevDependencies?: { [_: string]: string },

  startCommands?: StartCommand[],
}): Promise<{ [_ in keyof R]: string }> => {
  const { stage, configurationDir } = useGlobals();

  const tmpdir = useTempDir(props.id);

  const transformPath = pathTransformer(configurationDir, props.rootDir);

  entries(props.handlers).forEach(([_, v]) => {
    if (v.vars)
      useVarsWriter('ts', props.rootDir, v.vars);
  });

  const { dependencies, devDependencies } = entries(props.handlers).reduce((acc, [_, v]) => {
    try {
      const relPackageJsonPath = transformPath(v.packageJsonPath);
      const packagejson = JSON.parse(fs.readFileSync(relPackageJsonPath, { encoding: 'utf8' }));
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
  }, { dependencies: props.packageDependencies || {}, devDependencies: props.packageDevDependencies || {} });

  const maybePush = <T>(x: T | T[], r?: T[]): T[] =>
    Array.isArray(x) ? (r ? [...x, ...r] : x) : (r ? [x, ...r] : [x]);


  const makeHandler = (k: keyof R, v: Handler) => {
    const packageJsonPath = v.packageJsonPath.startsWith('/') ? v.packageJsonPath : path.resolve(`${props.rootDir}/${v.packageJsonPath}`);
    const relpath = path.relative(configurationDir, path.dirname(path.resolve(packageJsonPath)));
    if (stage == 'local')
      return `./build/${relpath}/${v.filepath.replace(/(.ts$)/, '')}.${v.entrypoint}`;
    else
      return `./build/${v.filepath.replace(/(.ts$)/, '')}.${v.entrypoint}`;
  }

  // generate serverless.yml
  const lam = {
    service: props.alias || `${path.basename(props.rootDir)}${props.id ? `--${props.id}` : ''}`,
    provider: {
      name: 'aws',
      runtime: 'nodejs10.x',
      stage,
      region: vars.AWS_REGION,
      vpc: props.slsVpc,
      iamRoleStatements: props.slsIamRoleStatements,
    },
    package: {
      include: maybePush(entries(props.handlers).map(([_, v]) => `build/${v.filepath.replace(/(.ts$)/, '.js')}`), props.slsIncludes),
      exclude: maybePush('**/*', props.slsExcludes)
    },
    functions: fromEntries(entries(props.handlers).map(([k, v]) => [k, {
      handler: makeHandler(k, v),
      environment: v.environment,
      events: v.events
    }])),
    //plugins: stage == 'local' ? ['serverless-offline', ...(inputs.slsPlugins || [])] : inputs.slsPlugins,
    ...(stage == 'local' ? { plugins: ['serverless-offline', ...(props.slsPlugins || [])] } : (props.slsPlugins ? { plugins: props.slsPlugins } : {}))
  };
  if (!SLSConfigRecord.guard(lam))
    throw new Error('invalid sls configuration');

  const dirnames = entries(props.handlers).map(([k, v]) => ({
    dirname: path.dirname(transformPath(v.packageJsonPath)),
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
    return path.relative(tmpdir, ROOTDIR);
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
    path: '${tmpdir}/build',
    filename: '_[name].js',
    libraryTarget: 'this'
  },
  ${props.webpackIgnore ? `plugins: [new webpack.IgnorePlugin(/${props.webpackIgnore.source}/)],` : ''}
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
        `${invReldir}/__tests__/**/*.ts`,
        `${invReldir}/**/__tests__/**/*.ts`,
      ]).flat(),
    ]
  };

  // generate package.json
  const packagejson = {
    "name": `@inf/tmp-${path.basename(props.rootDir)}${props.id ? `.${props.id}` : ''}`,
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
      "@inf/hookops": "*",
      ...(dependencies || {}),
    },
    "devDependencies": {
      "@types/jest": "24.0.18",
      "babel-jest": "24.8.0",
      "jest": "24.8.0",
      "jwk-to-pem": "2.0.1",
      "serverless": "1.48.4",
      "ts-jest": "24.0.2",
      "webpack": "4.28.4",
      "webpack-cli": "3.3.6",
      "concurrently": "4.1.2",
      "nodemon": "1.19.2",
      "serverless-offline": "5.11.0",
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
    return `'<rootDir>/${path.relative(tmpdir, ROOTDIR)}'`;
  }).join(',\n    ')}
  ],
  globals: {
    'ts-jest': {
      tsConfig: false
    }
  }
};
`;

  // generate makefile
  const startCmds: StartCommand[] = [];
  if (props.startCommands)
    props.startCommands.forEach(c => startCmds.push(c));

  if (startCmds.length == 0)
    startCmds.push({ command: 'yarn', args: ['-s', 'concurrently', '--kill-others', `"nodemon --watch ./build --exec 'yarn -s sls offline'"`, '"tsc -w -p tsconfig.sls.json"'] });

  const rules = {
    ...(startCmds.length && {
      start: {
        cwd: tmpdir,
        desc: 'starts server',
        commands: startCmds,
      }
    } || {}),
    ...fromEntries(entries(props.handlers).map(([k, v]) =>
      [[[`invoke.${k}`, {
        cwd: tmpdir,
        desc: 'invokes lambda function',
        commands: stage == 'local' ?
          [{
            command: `${path.relative(tmpdir, __dirname)}/bin/invokelocallambda`,
            args: [vars.AWS_REGION, `${props.id}-local-${k}`]
          }] :
          [{
            command: 'yarn',
            args: ['-s', 'vars', 'yarn', '-s', 'sls', 'invoke', '--function', k]
          }]
      }]]] as const
    ).flat().flat()),
    ...(stage == 'local' ? {} : fromEntries(entries(props.handlers).map(([k]) => [`logs.${k}`, {
      cwd: tmpdir,
      desc: 'tails out log messages',
      commands: [{ command: 'yarn', args: ['-s', 'vars', 'sls', 'logs', '-s', vars.STAGE, '-f', k, '-t', '-r', vars.AWS_REGION] }]
    }]))),
    build: {
      cwd: tmpdir,
      commands: [
        { command: 'cd', args: [tmpdir] },
        { command: 'yarn', args: ['-s', 'install', '--prefer-offline'] },
        { command: 'rm', args: ['-f', 'build/tsconfig.tsbuildinfo'] },
        { command: 'tsc', args: ['-p', 'tsconfig.sls.json'] },
        { command: 'yarn', args: ['-s', 'webpack', '--config', `${tmpdir}/webpack.config.js`] },
        { command: 'mkdir', args: ['-p', 'build/src'] },
        ...entries(props.handlers).map(([_, v]) =>
          ({ command: 'cp', args: [`build/_${path.basename(v.filepath.replace(/(.ts$)/, '.js'))}`, `build/${v.filepath.replace(/(.ts$)/, '.js')}`] }))]
    },
    deploy: {
      cwd: tmpdir,
      deps: ['build'],
      commands: [{ command: 'yarn', args: ['-s', 'vars', 'yarn', '-s', 'sls', 'deploy', '--no-confirm'] }]
    },
    ['.PHONY']: { deps: ['build', 'deploy'] }
  };

  useScriptRegistry(props.id, { rules });

  fs.writeFileSync(`${tmpdir}/serverless.yml`, yamljs.stringify(lam, Number.MAX_SAFE_INTEGER, 2), { encoding: 'utf8' });
  fs.writeFileSync(`${tmpdir}/webpack.config.js`, webpackconf, { encoding: 'utf8' });
  fs.writeFileSync(`${tmpdir}/tsconfig.sls.json`, JSON.stringify(tsconfig, null, 2), { encoding: 'utf8' });
  fs.writeFileSync(`${tmpdir}/package.json`, JSON.stringify(packagejson, null, 2), { encoding: 'utf8' });
  fs.writeFileSync(`${tmpdir}/jest.config.js`, jestconfig, { encoding: 'utf8' });

  //const dependsOn = [
  //...entries(inputs.handlers).map(([_, v]) => path.resolve(v.packageJsonPath)),
  //...entries(inputs.handlers).map(([_, v]) => `${path.resolve(path.dirname(v.packageJsonPath))}/**/*.ts`),
  //...(inputs.dependsOn || [])];

  const buildOutputMatchers = (handlers: { [_ in keyof R]: Handler }): { [_ in keyof R]: RegExp } => {
    const ents = entries(handlers);
    const fmapped = ents.map(([k, v]) =>
      v.events ? v.events.map(_ =>
        [k, /Service Information[\s\S.]+endpoints:[\s\S.]+POST - (.+)$/gm]) :
        [[k, new RegExp(`Service Information[\\s\\S]+functions:[\\s\\S]+${k}: (.+)`, 'gm')]] as const
    ).flat();
    return fromEntries(fmapped);
  }

  if (stage != 'local') {
    let dirty = false;

    await useDependsOn(async () => {
      dirty = true;
    }, props.dependsOn && props.dependsOn.map(transformPath));

    await useEffect(async () => {
      dirty = true;
    }, [fromEntries(entries(props.handlers).map(([k, v]) => [k, v.vars || {}]))])

    const ret: { [_ in keyof R]: string } = await useCache(async () => {
      await useShell({ command: 'yarn', args: ['-s', 'install', '--prefer-offline'], cwd: tmpdir });
      await useShell({ command: 'rm', args: ['-f', 'build/tsconfig.tsbuildinfo'], cwd: tmpdir });
      await useShell({ command: 'tsc', args: ['-p', 'tsconfig.sls.json'], cwd: tmpdir });
      await useShell({ command: 'yarn', args: ['-s', 'webpack', '--config', `${tmpdir}/webpack.config.js`], cwd: tmpdir });
      await useShell({ command: 'mkdir', args: ['-p', 'build/src'], cwd: tmpdir });
      for (let p of entries(props.handlers).map(([_, v]) => v.filepath))
        await useShell({ command: 'cp', args: [`build/_${path.basename(p.replace(/(.ts$)/, '.js'))}`, `build/${p.replace(/(.ts$)/, '.js')}`], cwd: tmpdir });

      return await useShell({
        command: 'yarn',
        args: ['-s', 'vars', 'yarn', '-s', 'sls', 'deploy', '--no-confirm'],
        cwd: tmpdir,
        outputMatchers: buildOutputMatchers(props.handlers),
      });
    }, dirty);
    return ret;
  } else {
    await useShell({
      command: 'yarn',
      args: ['-s', 'install', '--prefer-offline'],
      dependsOn: props.dependsOn && props.dependsOn.map(transformPath),
      cwd: tmpdir
    });

    const buildDefaultReturns = (stage: string, handlers: { [_ in keyof R]: Handler }) =>
      fromEntries(entries(handlers).filter(([k, v]) => !!v.events).map<[keyof R, string][]>(([k, v]) =>
        v.events!.map(e => [k, stage == 'local' ? `http://0.0.0.0:3000/${e.http.path}` : ''])).flat());

    return buildDefaultReturns(stage, props.handlers);
  }

};

