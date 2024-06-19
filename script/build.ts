import { Util } from '@mvanvu/ujs';
import * as prompts from 'prompts';
import * as fs from 'fs';
import { spawn } from 'child_process';

(async () => {
   const microservice = ['user', 'system', 'content', 'mailer', 'order', 'storage'];
   const buildFor = ['all', 'gateway', 'microservice'];
   const { buildFor: fIndex } = await prompts({
      type: 'select',
      name: 'buildFor',
      message: 'Choose a service',
      choices: buildFor.map((service) => ({ title: Util.uFirst(service) })),
   });

   const sources: { gateway: string[]; microservice: string[] } = { gateway: [], microservice: [] };

   switch (buildFor[fIndex]) {
      case 'gateway':
         sources.gateway.push('system', 'user', 'mailer', 'storage'); // Add requirement services
         const gatewayServices = microservice.filter((srv) => !['user', 'system', 'mailer', 'storage'].includes(srv));
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

   const cwd = process.cwd();
   const sourceBase = `${cwd}/src`;
   const buildDir = `${cwd}/build`;

   if (fs.existsSync(buildDir)) {
      fs.rmSync(buildDir, { recursive: true });
   }

   const copyFiles = (path: string | string[], buildDir: string, type: 'gateway' | 'microservice') => {
      if (Array.isArray(path)) {
         path.forEach((p) => copyFiles(p, buildDir, type));
      } else if (fs.existsSync(path)) {
         const stat = fs.statSync(path);

         if (stat.isDirectory()) {
            fs.readdirSync(path).forEach((dir) => copyFiles(`${path}/${dir}`, buildDir, type));
         } else if (stat.isFile()) {
            const dest = path.replace(cwd, buildDir);
            const dir = Util.dirName(dest);

            if (!fs.existsSync(dir)) {
               fs.mkdirSync(dir, { recursive: true });
            }

            fs.copyFileSync(path, dest);
         }
      }
   };
   const baseFiles: string[] = [
      `${sourceBase}/lib`,
      `${sourceBase}/config.ts`,
      `${sourceBase}/main.ts`,
      `${sourceBase}/metadata.ts`,
      `${cwd}/.env`,
      `${cwd}/nest-cli.json`,
      `${cwd}/package.json`,
      `${cwd}/tsconfig.json`,
      `${cwd}/yarn.lock`,
   ];

   if (sources.gateway.length) {
      const files = [...baseFiles, `${sourceBase}/gateway/lib`, `${sourceBase}/gateway/app.module.ts`];
      sources.gateway.forEach((srv) => files.push(`${sourceBase}/gateway/${srv}`));
      copyFiles(files, `${buildDir}/api-gateway`, 'gateway');
   }

   if (sources.microservice.length) {
      sources.microservice.forEach((srv) => {
         copyFiles(
            [...baseFiles, `${sourceBase}/microservice/lib`, `${sourceBase}/microservice/${srv}`],
            `${buildDir}/microservice/${srv}`,
            'microservice',
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
      console.log(`================= START TO BUILD ${source.toUpperCase()} =================`);
      const scripts: string[] = [`cd build/${source}`, '&& yarn'];

      if (source !== 'api-gateway') {
         const pkgJson = JSON.parse(fs.readFileSync(`${buildDir}/${source}/package.json`).toString());
         const srv = source.split('/')[1];

         if (pkgJson[`${srv}:prisma:generate`]) {
            scripts.push(`&& yarn ${srv}:prisma:generate`);
         }
      }

      scripts.push(`&& yarn nest:build`);
      const build = spawn(scripts.join(' '), { shell: true });

      await new Promise((resolve) => {
         build.stdout.on('data', (data) => console.log(`${data}`));
         build.stderr.on('data', (data) => console.log(`${data}`));
         build.on('close', (code) => {
            console.log(`Build ${source.toUpperCase()} ${code === 0 ? 'successfully' : 'ERROR'}`);
            resolve(code);
         });
      });
   }
})();
