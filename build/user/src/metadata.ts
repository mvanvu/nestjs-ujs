import { type INestMicroservice } from '@nestjs/common';
import { type NestExpressApplication } from '@nestjs/platform-express';
import { EventEmitter, Registry } from '@mvanvu/ujs';
import { loadPermissionKeys } from './lib';
import { ClientProxy } from '@nestjs/microservices';
export { appConfig } from './config';

// ==== DON'T CHANGE THIS BLOCK BELOW, BECAUSE IT WILL USED TO BUILD THE PRODUCTION SOURCE CODE =========
import user from './microservice/user/config';
const serviceConfigData = { user };

export const serviceConfig = Registry.from<typeof serviceConfigData>(serviceConfigData, { consistent: true });

export const serviceListNames = Object.entries(serviceConfigData).map(([, serviceConfig]) => serviceConfig.name);

export type ServiceName = (typeof serviceListNames)[number];

export const permissionKeys: string[] = [];

let _app: NestExpressApplication | INestMicroservice;

export const bootstrap = (app: NestExpressApplication | INestMicroservice): void => {
   _app = app;

   for (const service in serviceConfigData) {
      if (serviceConfigData[service].permissions) {
         loadPermissionKeys(serviceConfigData[service].permissions, permissionKeys);
      }
   }
};

export const app = <
   T extends 'Gateway' | 'Service' = undefined,
   R = T extends 'Gateway'
      ? NestExpressApplication
      : T extends 'Service'
        ? INestMicroservice
        : NestExpressApplication | INestMicroservice,
>(): R => <R>_app;

export const isGateway = (): boolean => process.env.APP_ENV === 'gateway';
export const isMicroservice = (): boolean => !isGateway();
export const injectProxy = (serviceName: ServiceName): string => `${serviceName.toUpperCase()}_MICROSERVICE`;
export const clientProxy = (serviceName: ServiceName): ClientProxy =>
   app<'Gateway'>().get<ClientProxy>(injectProxy(serviceName));

export const eventEmitter = new EventEmitter();
