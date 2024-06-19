import { ObjectRecord, Util } from '@mvanvu/ujs';
import * as prompts from 'prompts';
import * as fs from 'fs';
import { spawn } from 'child_process';

(async () => {
   const cwd = process.cwd();
   const buildFile = fs.existsSync(`${cwd}/build.json`) ? `${cwd}/build.json` : `${cwd}/build.sample.json`;
   const buildConfig = JSON.parse(fs.readFileSync(buildFile).toString());
   const microservice = buildConfig.microservice.list;
   const buildFor = ['all', 'api-gateway', 'microservice'];
   const { buildFor: fIndex } = await prompts({
      type: 'select',
      name: 'buildFor',
      message: 'Build for',
      choices: buildFor.map((service) => ({ title: Util.uFirst(service) })),
   });

   const sources: { gateway: string[]; microservice: string[] } = { gateway: [], microservice: [] };

   switch (buildFor[fIndex]) {
      case 'api-gateway':
         const defaultServices = buildConfig[buildFor[fIndex]].defaultList as string[];
         sources.gateway.push(...defaultServices); // Add requirement services
         const gatewayServices = microservice.filter((srv: string) => !defaultServices.includes(srv));
         const indexs = (
            await prompts({
               type: 'multiselect',
               name: 'gatewayServices',
               message: 'Choose some services',
               choices: gatewayServices.map((service) => ({ title: Util.uFirst(service) })),
               hint: '- Space to select. Return to submit',
            })
         ).gatewayServices as number[];

         if (indexs.length) {
            indexs.forEach((idx) => sources.gateway.push(gatewayServices[idx]));
         }

         break;

      case 'microservice':
         const serviceIndexs = (
            await prompts({
               type: 'multiselect',
               name: 'services',
               message: 'Choose some services',
               choices: microservice.map((service: string) => ({ title: Util.uFirst(service) })),
            })
         ).services as number[];

         if (serviceIndexs.length) {
            serviceIndexs.forEach((idx) => sources.microservice.push(microservice[idx]));
         } else {
            console.error(`Please choose one or some services`);
            process.exit();
         }

         break;

      case 'all':
         sources.gateway.push(...microservice);
         sources.microservice.push(...microservice);
         break;
   }

   console.log('STARTING TO BUILD, PLEASE WAIT...');
   const sourcesDir: string[] = [];
   const sourceBase = `${cwd}/src`;
   const buildDir = `${cwd}/build`;
   const buildScripts: Record<string, string[]> = {};
   const copyFiles = (path: string | string[], buildDir: string, servicesConfig: string[]) => {
      if (Array.isArray(path)) {
         path.forEach((p) => copyFiles(p, buildDir, servicesConfig));
      } else if (fs.existsSync(path)) {
         const stat = fs.statSync(path);

         if (stat.isDirectory()) {
            fs.readdirSync(path).forEach((dir) => copyFiles(`${path}/${dir}`, buildDir, servicesConfig));
         } else if (stat.isFile()) {
            const dest = path.replace(cwd, buildDir);
            const dir = Util.dirName(dest);

            if (!fs.existsSync(dir)) {
               fs.mkdirSync(dir, { recursive: true });
            }

            const appEnv = buildDir.split('/').pop();

            if (path === `${cwd}/.env`) {
               // Preprogress env
               const envConfig: ObjectRecord = {};

               for (let line of fs
                  .readFileSync(path)
                  .toString()
                  .split(/\n|\r\n/g)) {
                  line = line.trim();

                  if (!line.startsWith('#') && line.includes('=')) {
                     const parts = line.split('=');
                     const k = parts.shift();
                     const v = parts.join('=');
                     envConfig[k] = v;
                  }
               }

               envConfig.APP_ENV = `"${appEnv}"`;
               envConfig.NODE_ENV = '"production"';
               const envOverride = buildConfig.env?.[appEnv] || {};

               for (const k in envOverride) {
                  envConfig[k] = envOverride[k];
               }

               fs.writeFileSync(
                  dest,
                  Object.entries(envConfig)
                     .map(([k, v]) => `${k}=${v}`)
                     .join('\r\n'),
               );
            } else if (path === `${cwd}/package.json`) {
               const pks = JSON.parse(fs.readFileSync(path).toString());
               const dependencies = JSON.parse(JSON.stringify(pks.dependencies));
               pks.name += `-${appEnv === 'api-gateway' ? appEnv : `${appEnv}-microservice`}`;
               const scripts = {
                  build: 'nest build',
                  start: 'nest start',
                  'start:prod': 'node dist/main',
                  format: 'prettier --write "./**/src/**/*.ts"',
               };

               for (const script in pks.scripts) {
                  if (
                     /^(docker|mongodb):/.test(script) ||
                     (appEnv === 'api-gateway' && /prisma:/.test(script)) ||
                     (appEnv !== 'api-gateway' && new RegExp(`^${appEnv}:prisma:`).test(script))
                  ) {
                     scripts[script] = pks.scripts[script];
                  }
               }

               if (!buildScripts[appEnv]) {
                  buildScripts[appEnv] = [];
               }

               pks.scripts = scripts;

               if (pks.scripts[`${appEnv}:prisma:generate`]) {
                  buildScripts[appEnv].push(`yarn ${appEnv}:prisma:generate`);
               }

               const cfgKey = appEnv === 'api-gateway' ? 'apiGateway' : 'microservice';
               const pkgExcludes = buildConfig[cfgKey]?.dependencies?.exclude || [];
               const pkgIncludes = buildConfig[cfgKey]?.dependencies?.include || [];

               for (const excl of pkgExcludes) {
                  const [service, pkg] = excl.includes(':') ? excl.split(':') : [null, excl];

                  if (!service || service === appEnv) {
                     delete pks.dependencies[pkg];
                     delete pks.devDependencies[pkg];
                  }
               }

               for (const excl of pkgIncludes) {
                  const [service, pkg] = excl.includes(':') ? excl.split(':') : [null, excl];

                  if (!service || service === appEnv) {
                     pks.dependencies[pkg] = dependencies[pkg];
                  }
               }

               fs.writeFileSync(dest, JSON.stringify(pks, null, 2));
            } else if (path === `${sourceBase}/metadata.ts`) {
               const lines = fs
                  .readFileSync(path)
                  .toString()
                  .split(/\n|\r\n/);
               const output: string[] = [];
               let loadStart = false;
               let loadEnd = false;

               for (const line of lines) {
                  if (line.includes(`START TO LOAD THE MICROSERVICE CONFIGUARATION, DON'T REMOVE THIS LINE`)) {
                     loadStart = true;
                     output.push(
                        line,
                        ...servicesConfig.map((srv) => `import ${srv} from './lib/microservice/${srv}/config';`),
                     );
                     output.push(`const serviceConfigData = { ${servicesConfig.join(', ')} };`);
                     continue;
                  }

                  if (line.includes(`END TO LOAD THE MICROSERVICE CONFIGUARATION, DON'T REMOVE THIS LINE`)) {
                     loadEnd = true;
                     output.push(line);
                     continue;
                  }

                  if (!loadStart || loadEnd) {
                     output.push(line);
                  }
               }

               fs.writeFileSync(dest, output.join('\r\n'));
            } else {
               fs.copyFileSync(path, dest);
            }
         }
      }
   };
   const baseFiles: string[] = [
      `${cwd}/docker/Dockerfile`,
      `${sourceBase}/lib/common`,
      `${sourceBase}/config.ts`,
      `${sourceBase}/main.ts`,
      `${sourceBase}/metadata.ts`,
      `${cwd}/.env`,
      `${cwd}/.eslintrc.js`,
      `${cwd}/.gitignore`,
      `${cwd}/.prittierignore`,
      `${cwd}/.prettierrc`,
      `${cwd}/docker-compose.yml`,
      `${cwd}/nest-cli.json`,
      `${cwd}/package.json`,
      `${cwd}/tsconfig.json`,
      `${cwd}/yarn.lock`,
   ];

   if (sources.gateway.length) {
      if (fs.existsSync(`${buildDir}/api-gateway`)) {
         fs.rmSync(`${buildDir}/api-gateway`, { recursive: true });
      }

      sourcesDir.push('api-gateway');
      const files = [...baseFiles, `${sourceBase}/api-gateway/lib`, `${sourceBase}/api-gateway/app.module.ts`];
      sources.gateway.forEach((srv) =>
         files.push(`${sourceBase}/lib/microservice/${srv}`, `${sourceBase}/api-gateway/${srv}`),
      );
      copyFiles(files, `${buildDir}/api-gateway`, sources.gateway);
   }

   if (sources.microservice.length) {
      sources.microservice.forEach((srv) => {
         if (fs.existsSync(`${buildDir}/microservice/${srv}`)) {
            fs.rmSync(`${buildDir}/microservice/${srv}`, { recursive: true });
         }

         sourcesDir.push(`microservice/${srv}`);
         copyFiles(
            [
               ...baseFiles,
               `${sourceBase}/lib/microservice/${srv}`,
               `${sourceBase}/microservice/lib`,
               `${sourceBase}/microservice/${srv}`,
            ],
            `${buildDir}/microservice/${srv}`,
            [srv],
         );
      });
   }

   let successAll = true;

   for (const source of sourcesDir) {
      const scripts: string[] = [`cd build/${source}`, 'yarn'];
      const appEnv = source.split('/').pop();

      if (buildScripts[appEnv]?.length) {
         scripts.push(...buildScripts[appEnv]);
      }

      scripts.push(`yarn build`);
      const build = spawn(scripts.join(' && '), { shell: true });
      await new Promise((resolve) => {
         build.stdout.on('data', (data) => console.log(`${data}`));
         build.stderr.on('data', (data) => console.log(`${data}`));
         build.on('close', (code) => {
            if (code) {
               successAll = false;
            }

            const warnStr = `================= BUILD ${source.toUpperCase()} ${code === 0 ? 'SUCCESSFULLY' : 'FAILURE'} =================`;
            const repeatStr = '='.repeat(warnStr.length);
            console.log(`${repeatStr}\r\n${warnStr}\r\n${repeatStr}`);
            resolve(code);
         });
      });
   }

   const warnStr = `================= BUILD ${successAll ? ' ALL SUCCESSFULLY' : ' HAS SOME FAILURE'} =================`;
   const repeatStr = '='.repeat(warnStr.length);
   console.log(`${repeatStr}\r\n${warnStr}\r\n${repeatStr}`);
})();
