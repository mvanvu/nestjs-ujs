import { ClientsModule, ClientsProviderAsyncOptions, Transport } from '@nestjs/microservices';
import { appConfig } from '@config';
import { serviceListNames } from '@lib/service';

export function createClientAsyncOptions(name: string): ClientsProviderAsyncOptions {
   return {
      name: name.toUpperCase() + '_MICROSERVICE',
      useFactory: () => {
         return {
            transport: Transport.RMQ,
            options: {
               urls: [appConfig.get<string>('rabbitMQ.url')],
               queue: `${name}Queue`,
               queueOptions: { durable: true },
            },
         };
      },
   };
}

export default serviceListNames.map((name) =>
   ClientsModule.registerAsync({ isGlobal: true, clients: [createClientAsyncOptions(name)] }),
);
