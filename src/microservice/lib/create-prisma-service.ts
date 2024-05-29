import { ObjectRecord, Util } from '@mvanvu/ujs';
import { ClassConstructor } from '@lib/common';
import { OnApplicationShutdown, OnModuleInit } from '@nestjs/common';
import { CRUDService } from './service.crud';
import { PrismaClient } from '@prisma/client';

export function CreatePrismaService<TDataModel extends ObjectRecord>(
   PrismaClientRef: ClassConstructor<PrismaClient>,
   dataModel: TDataModel,
) {
   type ModelName = keyof TDataModel;

   class BasePrismaService extends PrismaClientRef implements OnModuleInit, OnApplicationShutdown {
      get models(): TDataModel {
         return dataModel;
      }

      async onModuleInit() {
         process.on('beforeExit', this.onApplicationShutdown);
         await this.$connect().catch(Util.debugDev);
      }

      async onApplicationShutdown() {
         await this.$disconnect();
      }

      createCRUDService(model: ModelName): CRUDService<this> {
         return new CRUDService(this, <string>model);
      }
   }

   return BasePrismaService;
}
