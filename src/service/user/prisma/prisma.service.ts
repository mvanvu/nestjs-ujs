import { Injectable, OnApplicationShutdown, OnModuleInit } from '@nestjs/common';
import { PrismaClient, $Enums } from '.prisma/user';
import { Util } from '@mvanvu/ujs';

@Injectable()
export class PrismaService extends PrismaClient implements OnModuleInit, OnApplicationShutdown {
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
