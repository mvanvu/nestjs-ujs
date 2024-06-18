import { Util } from '@mvanvu/ujs';
import * as prompts from 'prompts';
import * as fs from 'fs';
import { spawn } from 'child_process';

(async () => {
   const services = ['content', 'mailer', 'order', 'storage', 'system', 'user'];
   const { service: index } = await prompts({
      type: 'select',
      name: 'service',
      message: 'Choose a service',
      choices: services.map((service) => ({ title: Util.uFirst(service) })),
   });

   const cwd = process.cwd();
   const service = services[index];
   const buildDir = `${cwd}/build/${service}`;
   console.log(`Building microservice: ${Util.uFirst(service)}...`);

   if (!fs.existsSync(buildDir)) {
      fs.mkdirSync(buildDir, { recursive: true });
   }

   const sourceBase = `${cwd}/src`;
   const copyFiles = (path: string | string[]) => {
      if (Array.isArray(path)) {
         path.forEach(copyFiles);
      } else {
         const stat = fs.statSync(path);

         if (stat.isDirectory()) {
            fs.readdirSync(path).forEach((dir) => copyFiles(`${path}/${dir}`));
         } else if (stat.isFile()) {
            const dest = path.replace(cwd, buildDir);
            const dir = Util.dirName(dest);

            if (!fs.existsSync(dir)) {
               fs.mkdirSync(dir, { recursive: true });
            }

            if (path === `${sourceBase}/metadata.ts`) {
               const output: string[] = [];
               const lines = fs
                  .readFileSync(path)
                  .toString()
                  .split(/\n|\r\n/g);
               let isStartedBlock = false;
               let isEndedBlock = false;

               for (const line of lines) {
                  if (line.includes('START MICROSERVICE CONFIG BLOCK')) {
                     output.push(`import ${service} from './microservice/${service}/config';`);
                     output.push(`const serviceConfigData = { ${service} };`);
                     isStartedBlock = true;
                     continue;
                  }

                  if (line.includes('END MICROSERVICE CONFIG BLOCK')) {
                     isEndedBlock = true;
                     continue;
                  }

                  if (isStartedBlock && !isEndedBlock) {
                     continue;
                  }

                  output.push(line);
               }

               fs.writeFileSync(dest, output.join('\r\n'));
            } else {
               fs.copyFileSync(path, dest);
            }
         }
      }
   };

   copyFiles([
      `${sourceBase}/lib`,
      `${sourceBase}/config.ts`,
      `${sourceBase}/main.ts`,
      `${sourceBase}/metadata.ts`,
      `${sourceBase}/microservice/lib`,
      `${sourceBase}/microservice/${service}`,
      `${cwd}/.env`,
      `${cwd}/nest-cli.json`,
      `${cwd}/package.json`,
      `${cwd}/tsconfig.json`,
   ]);

   const build = spawn(`cd build/${service} && yarn && yarn ${service}:prisma:generate && yarn run nest build`, {
      shell: true,
   });

   await new Promise((resolve) => {
      build.stdout.on('data', (data) => console.log(`${data}`));
      build.stderr.on('data', (data) => console.error(`${data}`));
      build.on('close', (code) => {
         console.log(`Build microservice: ${Util.uFirst(service)} ${code === 0 ? 'successfully' : 'ERROR'}`);
         resolve(code);
      });
   });
})();
