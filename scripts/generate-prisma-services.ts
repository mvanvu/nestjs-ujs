import { Prisma as system } from '.prisma/system';
import { Prisma as user } from '.prisma/user';
import { Prisma as storage } from '.prisma/storage';
import { Prisma as content } from '.prisma/content';
import { Prisma as order } from '.prisma/order';
import { DMMF } from '@prisma/client/runtime/library';
import { exec } from 'child_process';
import * as fs from 'fs';

const models = { system, user, storage, content, order };

for (const name in models) {
   const contentJson = {};
   const filePath = `${process.cwd()}/src/microservice/${name}/provider/prisma`;

   if (!fs.existsSync(filePath)) {
      fs.mkdirSync(filePath, { recursive: true });
   }

   models[name].dmmf.datamodel.models.forEach((model: DMMF.Model) => (contentJson[model.name] = model));

   // Write file data model
   fs.writeFileSync(
      `${filePath}/prisma.datamodel.ts`,
      `export const ${name}DataModels = ${JSON.stringify(contentJson, null, 2)};`,
   );

   // Write Prisma service
   fs.writeFileSync(
      `${filePath}/prisma.service.ts`,
      `
      import { Inject, Injectable } from '@nestjs/common';
      import { PrismaClient } from '.prisma/${name}';
      import { ${name}DataModels } from './prisma.datamodel';
      import { CreatePrismaService } from '@service/lib';
      import { CONTEXT, RequestContext } from '@nestjs/microservices';
      
      @Injectable()
      export class PrismaService extends CreatePrismaService(PrismaClient, ${name}DataModels) {
         @Inject(CONTEXT) readonly ctx: RequestContext;
      }
      `,
   );
}

exec('yarn format', (err) => {
   console.log(err ? `ERR: ${err}` : 'Success');
});
