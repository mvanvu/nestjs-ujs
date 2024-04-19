import { type INestMicroservice } from '@nestjs/common';
import { type NestExpressApplication } from '@nestjs/platform-express';

class Metadata {
   private appGateway: NestExpressApplication;

   private appService: INestMicroservice;

   getGateway(): NestExpressApplication {
      return this.appGateway;
   }

   setGateway(app: NestExpressApplication): this {
      if (!this.appGateway) {
         this.appGateway = app;
      }

      return this;
   }

   getService(): INestMicroservice {
      return this.appService;
   }

   setService(app: INestMicroservice): this {
      if (!this.appService) {
         this.appService = app;
      }

      return this;
   }

   isGateway(): boolean {
      return process.env.APP_ENV === 'gateway';
   }

   isMicroservice(): boolean {
      return !this.isGateway();
   }

   getApp(): NestExpressApplication | INestMicroservice {
      return this.isGateway() ? this.getGateway() : this.getService();
   }
}

export const metadata = new Metadata();
