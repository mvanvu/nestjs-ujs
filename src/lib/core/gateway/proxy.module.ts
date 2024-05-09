import * as serviceConfig from '@service/config';
import { ClientsModule, ClientsProviderAsyncOptions, Transport } from '@nestjs/microservices';
import { appConfig } from '@lib/core/config';

export function createClientAsyncOptions(name: string): ClientsProviderAsyncOptions {
   return {
      name,
      useFactory: () => {
         return {
            transport: Transport.RMQ,
            options: {
               urls: [appConfig.rabbitMQ.url],
               queue: `${name}Queue`,
               queueOptions: { durable: true },
            },
         };
      },
   };
}

// Todo add more services
const services = [serviceConfig.userConfig.proxy, serviceConfig.storageConfig.proxy];

export default services.map((name) =>
   ClientsModule.registerAsync({ isGlobal: true, clients: [createClientAsyncOptions(name)] }),
);
