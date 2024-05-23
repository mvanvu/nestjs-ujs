import { serviceListNames } from '@lib/service';
import { NotImplementedException, type INestMicroservice } from '@nestjs/common';
import { type NestExpressApplication } from '@nestjs/platform-express';

class Metadata {
   private appGateway: NestExpressApplication;

   private appService: Record<string, INestMicroservice> = {};

   validateServiceName(serviceName: string) {
      if (!serviceListNames.includes(serviceName)) {
         throw new NotImplementedException(`The serviceName must be in [${serviceListNames}]`);
      }
   }

   getGateway(): NestExpressApplication {
      return this.appGateway;
   }

   setGateway(app: NestExpressApplication): this {
      if (!this.appGateway) {
         this.appGateway = app;
      }

      return this;
   }

   getService(serviceName: string): INestMicroservice {
      this.validateServiceName(serviceName);

      return this.appService[serviceName];
   }

   setService(serviceName: string, app: INestMicroservice): this {
      this.validateServiceName(serviceName);
      this.appService[serviceName] = app;

      return this;
   }

   isGateway(): boolean {
      return process.env.APP_ENV === 'gateway';
   }

   isMicroservice(): boolean {
      return !this.isGateway();
   }
}

export const metadata = new Metadata();
