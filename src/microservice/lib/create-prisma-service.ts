import { ObjectRecord } from '@mvanvu/ujs';
import { ClassConstructor } from '@lib/common';
import { OnApplicationShutdown, OnModuleInit } from '@nestjs/common';
import { CRUDService } from './service.crud';
import { PrismaClient } from '@prisma/client';

export function CreatePrismaService<TClientRef extends ClassConstructor<PrismaClient>, TDataModel extends ObjectRecord>(
   PrismaClientRef: TClientRef,
   dataModels: TDataModel,
) {
   class BasePrismaService extends PrismaClientRef implements OnModuleInit, OnApplicationShutdown {
      get models(): TDataModel {
         return dataModels;
      }

      async onModuleInit() {
         process.on('beforeExit', this.onApplicationShutdown);
         await this.$connect().catch(console.debug);
      }

      async onApplicationShutdown() {
         await this.$disconnect();
      }

      createCRUDService(model: string): CRUDService<this> {
         return new CRUDService(this, <string>model, this['ctx']);
      }
   }

   Object.defineProperty(BasePrismaService, 'name', { value: BasePrismaService.name });

   return BasePrismaService;
}
