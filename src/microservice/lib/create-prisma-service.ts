import { ObjectRecord, Util } from '@mvanvu/ujs';
import { ClassConstructor } from '@lib/common';
import { OnApplicationShutdown, OnModuleInit } from '@nestjs/common';
import { CRUDService } from './service.crud';
import { PrismaClient } from '@prisma/client';
import { BaseRpcContext } from '@nestjs/microservices';

export function CreatePrismaService<TClientRef extends ClassConstructor<PrismaClient>, TDataModel extends ObjectRecord>(
   PrismaClientRef: TClientRef,
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
         const service = new CRUDService(this, <string>model);

         if (this['ctx']?.getContext() instanceof BaseRpcContext) {
            service.setCtx(this['ctx']);
         }

         return service;
      }
   }

   Object.defineProperty(BasePrismaService, 'name', { value: BasePrismaService.name });

   return BasePrismaService;
}
