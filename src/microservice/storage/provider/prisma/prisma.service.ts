import { Injectable, OnApplicationShutdown, OnModuleInit } from '@nestjs/common';
import { PrismaClient, Prisma } from '.prisma/storage';
import { Util } from '@mvanvu/ujs';
import { PrismaModels, GetPrismaModels } from '@lib/type';

@Injectable()
export class PrismaService extends PrismaClient implements GetPrismaModels, OnModuleInit, OnApplicationShutdown {
   private _models: PrismaModels;

   get models(): PrismaModels {
      if (!this._models) {
         this._models = {};
         Prisma.dmmf.datamodel.models.forEach((model: Prisma.DMMF.Model) => {
            this._models[model.name] = model;
         });
      }

      return this._models;
   }

   async onModuleInit() {
      process.on('beforeExit', this.onApplicationShutdown);
      await this.$connect().catch(Util.debugDev);
   }

   async onApplicationShutdown() {
      await this.$disconnect();
   }
}
