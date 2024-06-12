import { type INestMicroservice } from '@nestjs/common';
import { type NestExpressApplication } from '@nestjs/platform-express';
import { Registry } from '@mvanvu/ujs';
import { loadPermissionKeys } from './lib/common';
import { ClientProxy } from '@nestjs/microservices';

// Service shared config
import system from './lib/service/system/config';
import user from './lib/service/user/config';
import storage from './lib/service/storage/config';
import mailer from './lib/service/mailer/config';
import content from './lib/service/content/config';
import order from './lib/service/order/config';

export { appConfig } from './config';
const serviceConfigData = { system, mailer, storage, user, content, order };

export const serviceConfig = Registry.from<typeof serviceConfigData>(serviceConfigData, { consistent: true });

export const serviceListNames = [
   serviceConfig.get('system.name'),
   serviceConfig.get('mailer.name'),
   serviceConfig.get('user.name'),
   serviceConfig.get('storage.name'),
   serviceConfig.get('content.name'),
   serviceConfig.get('order.name'),
] as const;

export type ServiceName = (typeof serviceListNames)[number];

export const permissionKeys: string[] = [];

let _app: NestExpressApplication | INestMicroservice;

export const bootstrap = (app: NestExpressApplication | INestMicroservice): void => {
   _app = app;
   loadPermissionKeys(user.permissions, permissionKeys);
   loadPermissionKeys(storage.permissions, permissionKeys);
   loadPermissionKeys(system.permissions, permissionKeys);
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
