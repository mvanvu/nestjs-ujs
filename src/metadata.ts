import { type INestMicroservice } from '@nestjs/common';
import { type NestExpressApplication } from '@nestjs/platform-express';
import { EventEmitter, Registry } from '@mvanvu/ujs';
import { loadPermissionKeys } from './@shared-library';
import { ClientProxy } from '@nestjs/microservices';
export { appConfig } from './config';

// START TO LOAD THE MICROSERVICE CONFIGUARATION, DON'T REMOVE THIS LINE
import system from '@microservice/system/config';
import user from '@microservice/user/config';
import content from '@microservice/content/config';
const serviceConfigData = { system, user, content };
// END TO LOAD THE MICROSERVICE CONFIGUARATION, DON'T REMOVE THIS LINE

export const serviceConfig = Registry.from<typeof serviceConfigData>(serviceConfigData, { consistent: true });

export const serviceListNames = Object.entries(serviceConfigData).map(([, serviceConfig]) => serviceConfig.name);

export type ServiceName = (typeof serviceListNames)[number];

const permissionKeys: string[] = [];

export const getPermissionKeys = (): string[] => {
   if (!permissionKeys.length) {
      for (const service in serviceConfigData) {
         if (serviceConfigData[service].permissions) {
            loadPermissionKeys(serviceConfigData[service].permissions, permissionKeys);
         }
      }
   }

   return permissionKeys;
};
let _app: NestExpressApplication | INestMicroservice;

export const bootstrap = (app: NestExpressApplication | INestMicroservice): void => {
   _app = app;
};

export const app = <
   T extends 'Gateway' | 'Service' = undefined,
   R = T extends 'Gateway'
      ? NestExpressApplication
      : T extends 'Service'
        ? INestMicroservice
        : NestExpressApplication | INestMicroservice,
>(): R => <R>_app;

export const isGateway = (): boolean => process.env.APP_ENV === 'api-gateway';
export const isMicroservice = (): boolean => !isGateway();
export const injectProxy = (serviceName: ServiceName): string => `${serviceName.toUpperCase()}_MICROSERVICE`;
export const clientProxy = (serviceName: ServiceName): ClientProxy =>
   app<'Gateway'>().get<ClientProxy>(injectProxy(serviceName));

export const eventEmitter = new EventEmitter();
