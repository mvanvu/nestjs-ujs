import { ObjectRecord, Util } from '@mvanvu/ujs';
import * as fs from 'fs';
import * as dotenvx from '@dotenvx/dotenvx';

(async () => {
   const cwd = process.cwd();
   const buildFile = `${cwd}/script/build.json`;
   const buildConfig = JSON.parse(fs.readFileSync(buildFile).toString());
   const microservice = buildConfig.microservice.list;
   const sources: { gateway: string[]; microservice: string[] } = { gateway: [], microservice: [] };

   sources.gateway.push(...microservice);
   sources.microservice.push(...microservice);

   console.log('STARTING TO BUILD, PLEASE WAIT...');
   const sourcesDir: string[] = [];
   const sourceBase = `${cwd}/src`;
   const buildDir = `${cwd}/build`;
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
               fs.readFileSync(path)
                  .toString()
                  .split(/\r?\n/g)
                  .forEach((line) => {
                     if (line.includes('=')) {
                        const idx = line.indexOf('=');
                        const key = line.substring(0, idx);
                        const value = line.substring(idx + 1);
                        envConfig[key.trim()] = value.trim();
                     }
                  });

               if (buildConfig.env) {
                  if (buildConfig.env.$global) {
                     Object.assign(envConfig, buildConfig.env.$global);
                  }

                  const envOverride = buildConfig.env?.[appEnv] || null;

                  if (envOverride) {
                     Object.assign(envConfig, envOverride);
                  }
               }

               envConfig.APP_ENV = appEnv;
               envConfig.NODE_ENV = 'production';
               const toEnvContent = (obj: ObjectRecord): string =>
                  Object.entries(obj)
                     .map(([k, v]) => `${k}=${v}`)
                     .join('\r\n');

               // Then write .env file
               fs.writeFileSync(dest, toEnvContent(dotenvx.parse(toEnvContent(envConfig), { override: true })));
            } else if (path === `${cwd}/package.json`) {
               const orginPks = JSON.parse(fs.readFileSync(path).toString());
               const pks = JSON.parse(JSON.stringify(orginPks));
               const isGateway = appEnv === 'api-gateway';
               const pkgSuffix = isGateway ? appEnv : `${appEnv}-microservice`;
               pks.name += `-${pkgSuffix}`;
               const pckName = isGateway ? 'api-gateway' : 'microservice';
               const pkgExcludes = buildConfig[pckName]?.dependencies?.exclude || [];
               const pkgIncludes = buildConfig[pckName]?.dependencies?.include || [];

               for (const excl of pkgExcludes) {
                  const [service, pkg] = excl.includes(':') ? excl.split(':') : [null, excl];

                  if (!service || service === appEnv) {
                     delete pks.dependencies[pkg];
                     delete pks.devDependencies[pkg];
                     delete pks.devDependencies[`@types/${pkg}`];
                  }
               }

               for (const incl of pkgIncludes) {
                  const [service, pkg] = incl.includes(':') ? incl.split(':') : [null, incl];

                  if (!service || service === appEnv) {
                     if (orginPks.dependencies[pkg]) {
                        pks.dependencies[pkg] = orginPks.dependencies[pkg];
                     }

                     if (orginPks.devDependencies[pkg]) {
                        pks.devDependencies[pkg] = orginPks.devDependencies[pkg];
                     }

                     if (orginPks.devDependencies[`@types/${pkg}`]) {
                        pks.devDependencies[`@types/${pkg}`] = orginPks.devDependencies[`@types/${pkg}`];
                     }
                  }
               }

               const dockerImage = `${process.env.APP_NAME}-${pkgSuffix}-production`;
               pks.scripts = {
                  'docker:build': `dotenvx run -f .env -- docker build -t ${dockerImage} .`,
                  'docker:remove': `dotenvx run -f .env -- docker rm -f $(docker ps -aq -f "name=${dockerImage}") 2>/dev/null | true`,
                  'docker:start': `docker run -d --name ${dockerImage}${isGateway ? ` -p ${process.env.APP_PORT}:${process.env.APP_PORT}` : ''} --network ${process.env.APP_NAME}-network --env-file .env --restart always ${dockerImage}`,
                  deploy: 'yarn docker:build && yarn docker:remove && yarn docker:start',
                  start: 'dotenvx run -f .env -- nest start',
               };

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
                        ...servicesConfig.map((srv) => `import ${srv} from '@microservice/${srv}/config';`),
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
      `${sourceBase}/@shared-library`,
      `${sourceBase}/microservice/system/prisma`,
      `${sourceBase}/microservice/user/prisma`,
      `${sourceBase}/microservice/content/prisma`,
      `${sourceBase}/config.ts`,
      `${sourceBase}/main.ts`,
      `${sourceBase}/metadata.ts`,
      `${cwd}/.env`,
      `${cwd}/.eslintrc.js`,
      `${cwd}/.gitignore`,
      `${cwd}/.prittierignore`,
      `${cwd}/.prettierrc`,
      `${cwd}/nest-cli.json`,
      `${cwd}/package.json`,
      `${cwd}/tsconfig.json`,
      `${cwd}/yarn.lock`,
   ];

   const dockerFileBaseContent = [
      'FROM node:20',
      'WORKDIR /usr/src/app',
      'COPY . .',
      'RUN npm install',
      'RUN npx prisma generate --schema=src/microservice/system/prisma/schema.prisma',
      'RUN npx prisma generate --schema=src/microservice/user/prisma/schema.prisma',
      'RUN npx prisma generate --schema=src/microservice/content/prisma/schema.prisma',
   ];

   if (sources.gateway.length) {
      if (fs.existsSync(`${buildDir}/api-gateway`)) {
         fs.rmSync(`${buildDir}/api-gateway`, { recursive: true });
      }

      sourcesDir.push('api-gateway');
      const files = [...baseFiles, `${sourceBase}/api-gateway/@library`, `${sourceBase}/api-gateway/app.module.ts`];
      sources.gateway.forEach((srv) => {
         files.push(
            `${sourceBase}/api-gateway/${srv}`,
            `${sourceBase}/microservice/${srv}/config.ts`,
            `${sourceBase}/microservice/${srv}/dto`,
            `${sourceBase}/microservice/${srv}/entity`,
            `${sourceBase}/microservice/${srv}/type`,
         );
      });
      copyFiles(files, `${buildDir}/api-gateway`, sources.gateway);
      fs.writeFileSync(
         `${buildDir}/api-gateway/Dockerfile`,
         [
            ...dockerFileBaseContent,
            'RUN npx nest build',
            `EXPOSE ${process.env.APP_PORT || 9001}`,
            'CMD ["node", "dist/main"]',
         ].join('\r\n\r\n'),
      );
   }

   if (sources.microservice.length) {
      sources.microservice.forEach((srv) => {
         if (fs.existsSync(`${buildDir}/microservice/${srv}`)) {
            fs.rmSync(`${buildDir}/microservice/${srv}`, { recursive: true });
         }

         sourcesDir.push(`microservice/${srv}`);
         copyFiles(
            [...baseFiles, `${sourceBase}/microservice/@library`, `${sourceBase}/microservice/${srv}`],
            `${buildDir}/microservice/${srv}`,
            [srv],
         );
         fs.writeFileSync(
            `${buildDir}/microservice/${srv}/Dockerfile`,
            [...dockerFileBaseContent, 'RUN npx nest build', 'CMD ["node", "dist/main"]'].join('\r\n\r\n'),
         );
      });
   }
})();
