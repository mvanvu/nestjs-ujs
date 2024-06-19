import { Util } from '@mvanvu/ujs';
import * as prompts from 'prompts';
import * as fs from 'fs';
import { spawn } from 'child_process';

(async () => {
   // eslint-disable-next-line @typescript-eslint/no-var-requires
   const buildConfig = require('../build/config.json');
   const microservice = buildConfig.microservice.list;
   const buildFor = ['all', 'api-gateway', 'microservice'];
   const { buildFor: fIndex } = await prompts({
      type: 'select',
      name: 'buildFor',
      message: 'Choose a service',
      choices: buildFor.map((service) => ({ title: Util.uFirst(service) })),
   });

   const sources: { gateway: string[]; microservice: string[] } = { gateway: [], microservice: [] };

   switch (buildFor[fIndex]) {
      case 'api-gateway':
         const defaultServices = buildConfig.apiGateway.defaultServiceList as string[];
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
               choices: microservice.map((service) => ({ title: Util.uFirst(service) })),
            })
         ).services as number[];

         if (serviceIndexs.length) {
            serviceIndexs.forEach((idx) => sources.microservice.push(microservice[idx]));
         } else {
            console.error(`Please choose one or some services`);
            process.exit();
         }

         break;

      default:
      case 'all':
         sources.gateway.push(...microservice);
         sources.microservice.push(...microservice);
         break;
   }

   console.log('START TO BUILD, PLEASE WAIT...');
   const cwd = process.cwd();
   const sourceBase = `${cwd}/src`;
   const buildDir = `${cwd}/build`;
   const buildScripts: Record<string, string[]> = {};

   const copyFiles = (path: string | string[], buildDir: string) => {
      if (Array.isArray(path)) {
         path.forEach((p) => copyFiles(p, buildDir));
      } else if (fs.existsSync(path)) {
         const stat = fs.statSync(path);

         if (stat.isDirectory()) {
            fs.readdirSync(path).forEach((dir) => copyFiles(`${path}/${dir}`, buildDir));
         } else if (stat.isFile()) {
            const dest = path.replace(cwd, buildDir);
            const dir = Util.dirName(dest);

            if (!fs.existsSync(dir)) {
               fs.mkdirSync(dir, { recursive: true });
            }

            const appEnv = buildDir.split('/').pop();
            const fName = path.split('/').pop();

            if (fName === '.env') {
               // Preprogress env
               fs.writeFileSync(
                  dest,
                  `APP_ENV="${appEnv}"\r\n` +
                     fs
                        .readFileSync(path)
                        .toString()
                        .replace(/NODE_ENV="?([a-zA-Z]+)"?/, 'NODE_ENV="production"'),
               );
            } else if (fName === 'package.json') {
               const pks = JSON.parse(fs.readFileSync(path).toString());
               pks.name += `-${appEnv === 'gateway' ? appEnv : `${appEnv}-microservice`}`;
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

               fs.writeFileSync(dest, JSON.stringify(pks, null, 2));
            } else {
               fs.copyFileSync(path, dest);
            }
         }
      }
   };
   const baseFiles: string[] = [
      `${cwd}/docker/Dockerfile`,
      `${sourceBase}/lib`,
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

      const files = [...baseFiles, `${sourceBase}/api-gateway/lib`, `${sourceBase}/api-gateway/app.module.ts`];
      sources.gateway.forEach((srv) => files.push(`${sourceBase}/api-gateway/${srv}`));
      copyFiles(files, `${buildDir}/api-gateway`);
   }

   if (sources.microservice.length) {
      sources.microservice.forEach((srv) => {
         if (fs.existsSync(`${buildDir}/microservice/${srv}`)) {
            fs.rmSync(`${buildDir}/microservice/${srv}`, { recursive: true });
         }

         copyFiles(
            [...baseFiles, `${sourceBase}/microservice/lib`, `${sourceBase}/microservice/${srv}`],
            `${buildDir}/microservice/${srv}`,
         );
      });
   }

   const sourcesDir: string[] = [];

   if (fs.existsSync(`${buildDir}/api-gateway`)) {
      sourcesDir.push('api-gateway');
   }

   if (fs.existsSync(`${buildDir}/microservice`)) {
      sourcesDir.push(...fs.readdirSync(`${buildDir}/microservice`).map((dir) => `microservice/${dir}`));
   }

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
            const warnStr = `================= BUILD ${source.toUpperCase()} ${code === 0 ? 'SUCCESSFULLY' : 'FAILURE'} =================`;
            const repeatStr = '='.repeat(warnStr.length);
            console.log(`${repeatStr}\r\n${warnStr}\r\n${repeatStr}`);
            resolve(code);
         });
      });
   }
})();
