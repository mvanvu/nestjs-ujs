import { Prisma as user } from '.prisma/user';
import { Prisma as storage } from '.prisma/storage';
import { DMMF } from '@prisma/client/runtime/library';
import { exec } from 'child_process';
import * as fs from 'fs';

const models = { user, storage };

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
      import { Injectable } from '@nestjs/common';
      import { PrismaClient } from '.prisma/${name}';
      import { ${name}DataModels } from './prisma.datamodel';
      import { CreatePrismaService } from '@service/lib';
      
      @Injectable()
      export class PrismaService extends CreatePrismaService(PrismaClient, ${name}DataModels) {}
      `,
   );
}

exec('yarn format', (err) => {
   console.log(err ? `ERR: ${err}` : 'Success');
});
