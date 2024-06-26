import { ObjectRecord } from '@mvanvu/ujs';
import { ClassConstructor, ThrowException } from '@shared-library';
import { HttpStatus, OnApplicationShutdown, OnModuleInit } from '@nestjs/common';
import { CRUDService } from './service.crud';
import { PrismaClient } from '@prisma/client';

export function CreatePrismaService<
   TClientRef extends ClassConstructor<PrismaClient>,
   TDataModel extends ObjectRecord[],
>(PrismaClientRef: TClientRef, prismaModels: TDataModel) {
   const dataModels: Record<string, any> = {};
   prismaModels.forEach((model) => (dataModels[model.name] = model));

   class BasePrismaService extends PrismaClientRef implements OnModuleInit, OnApplicationShutdown {
      get models(): Record<string, any> {
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
         if (!this.models[model]) {
            ThrowException(`The model(${model}) doesn't exists`, HttpStatus.NOT_IMPLEMENTED);
         }

         return new CRUDService(this, <string>model, this['meta']);
      }
   }

   Object.defineProperty(BasePrismaService, 'name', { value: BasePrismaService.name });

   return BasePrismaService;
}
