import { ConfigService } from '@nestjs/config';
import { ClientsModule, ClientsProviderAsyncOptions, Transport } from '@nestjs/microservices';

export function createClientAsyncOptions(name: string): ClientsProviderAsyncOptions {
   return {
      name,
      inject: [ConfigService],
      useFactory: (config: ConfigService) => {
         return {
            transport: Transport.RMQ,
            options: {
               urls: [config.get<string>('RABBITMQ_URL')],
               queue: `${name}Queue`,
               queueOptions: { durable: true },
            },
         };
      },
   };
}

// Todo add more services
const services = ['UserProxy'];

export default services.map((name) =>
   ClientsModule.registerAsync({ isGlobal: true, clients: [createClientAsyncOptions(name)] }),
);
