import { type INestMicroservice } from '@nestjs/common';
import { type NestExpressApplication } from '@nestjs/platform-express';
import { Registry } from '@mvanvu/ujs';
import { loadPermissionKeys } from './common';

// Service shared config
import user from './service/user/config';
import storage from './service/storage/config';
const serviceConfigData = { user, storage };
type ServiceConfigData = typeof serviceConfigData;

class Metadata {
   private app: NestExpressApplication | INestMicroservice;

   readonly serviceConfig = Registry.from<ServiceConfigData>(serviceConfigData, { consistent: true });

   readonly serviceListNames = [this.serviceConfig.get('user.name'), this.serviceConfig.get('storage.name')];

   readonly permissionKeys: string[] = [];

   bootstrap(app: NestExpressApplication | INestMicroservice): void {
      this.app = app;
      loadPermissionKeys(user.permissions, this.permissionKeys);
      loadPermissionKeys(storage.permissions, this.permissionKeys);
   }

   getApp<T extends 'Gateway' | 'Service', R = T extends 'Gateway' ? NestExpressApplication : INestMicroservice>(): R {
      return <R>this.app;
   }

   isGateway(): boolean {
      return process.env.APP_ENV === 'gateway';
   }

   isMicroservice(): boolean {
      return !this.isGateway();
   }
}

export const metadata = new Metadata();
