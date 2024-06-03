import { ClientsModule, ClientsProviderAsyncOptions, Transport } from '@nestjs/microservices';
import { appConfig, serviceListNames } from '@metadata';

export function createClientAsyncOptions(name: string): ClientsProviderAsyncOptions {
   return {
      name: name.toUpperCase() + '_MICROSERVICE',
      useFactory: () => {
         return {
            transport: Transport.RMQ,
            options: {
               urls: [appConfig.get<string>('rabbitMQ.url')],
               queue: `${name}MicroserviceQueue`,
               queueOptions: { durable: true },
            },
         };
      },
   };
}

export const clientProxies = serviceListNames.map((name) =>
   ClientsModule.registerAsync({ isGlobal: true, clients: [createClientAsyncOptions(name)] }),
);
