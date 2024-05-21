import { PrismaClient } from '@prisma/client';
import { Util } from '@mvanvu/ujs';
import { PrismaModels, ClassConstructor, GetPrismaModels, PrismaModelName } from '@lib/common';
import { DMMF } from '@prisma/client/runtime/library';
import { OnApplicationShutdown, OnModuleInit } from '@nestjs/common';
import { CRUDService } from './service.crud';

export function CreatePrismaService(ClientRef: ClassConstructor<PrismaClient>, dataModels: DMMF.Datamodel['models']) {
   class PrismaClientRef extends ClientRef implements GetPrismaModels, OnModuleInit, OnApplicationShutdown {
      #models: PrismaModels;

      get models(): PrismaModels {
         if (!this.#models) {
            this.#models = {};
            dataModels.forEach((model: DMMF.Model) => {
               this.#models[model.name] = model;
            });
         }

         return this.#models;
      }

      async onModuleInit() {
         process.on('beforeExit', this.onApplicationShutdown);
         await this.$connect().catch(Util.debugDev);
      }

      async onApplicationShutdown() {
         await this.$disconnect();
      }

      createCRUD(model: PrismaModelName<this>): CRUDService<this> {
         return new CRUDService(this, model);
      }
   }

   return PrismaClientRef;
}
