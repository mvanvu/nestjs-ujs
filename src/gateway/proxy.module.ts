import { ClientsModule, ClientsProviderAsyncOptions, Transport } from '@nestjs/microservices';
import { appConfig } from '@config';
import { clientProxies } from '@lib';

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

export default clientProxies.map((name) =>
   ClientsModule.registerAsync({ isGlobal: true, clients: [createClientAsyncOptions(name)] }),
);
