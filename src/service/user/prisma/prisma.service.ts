import { Injectable, OnApplicationShutdown, OnModuleInit } from '@nestjs/common';
import { PrismaClient, $Enums, Prisma } from '.prisma/user';
import { Util } from '@mvanvu/ujs';
import { BasePrismaService } from '@lib';

@Injectable()
export class PrismaService extends PrismaClient implements BasePrismaService, OnModuleInit, OnApplicationShutdown {
   readonly dmmf = Prisma.dmmf;

   get enums() {
      return $Enums;
   }

   async onModuleInit() {
      process.on('beforeExit', this.onApplicationShutdown);
      await this.$connect().catch(Util.debugDev);
   }

   async onApplicationShutdown() {
      await this.$disconnect();
   }
}
