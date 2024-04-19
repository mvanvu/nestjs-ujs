import { INestMicroservice } from '@nestjs/common';
import { type NestExpressApplication } from '@nestjs/platform-express';
import { ClassConstructor } from '@lib/type';

class Metadata {
   private appGateway: NestExpressApplication;

   private appService: INestMicroservice;

   private messagePatterns: Record<string, [Object, string]> = {};

   getGateway(): NestExpressApplication {
      return this.appGateway;
   }

   setGateway(app: NestExpressApplication) {
      this.appGateway = app;
   }

   getService(): INestMicroservice {
      return this.appService;
   }

   setService(app: INestMicroservice) {
      this.appService = app;
   }

   registerPattern(pattern: string, target: Object, method: string): void {
      this.messagePatterns[pattern] = [target, method];
   }

   getPatterns(): Record<string, [Object, string]> {
      return this.messagePatterns;
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

   getPattern<T>(target: ClassConstructor<T>, method: string): string | null {
      const patterns = this.getPatterns();

      for (const pattern in patterns) {
         if (patterns[pattern][0] === target && patterns[pattern][1] === method) {
            return pattern;
         }
      }

      return null;
   }
}

export const metadata = new Metadata();
