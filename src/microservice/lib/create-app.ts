import { ClassConstructor, ValidationPipe, metadata, ExceptionFilter } from '@lib';
import { Callable, Util } from '@mvanvu/ujs';
import { NestFactory } from '@nestjs/core';
import { MicroserviceOptions, Transport } from '@nestjs/microservices';
import { appConfig } from '../../config';

export async function createMicroserviceApp(
   AppModule: ClassConstructor<any>,
   queue: string,
   onBeforeListen?: Callable,
) {
   const app = await NestFactory.createMicroservice(AppModule, <MicroserviceOptions>{
      transport: Transport.RMQ,
      options: { urls: [appConfig.get('rabbitMQ.url')], queue, queueOptions: { durable: true } },
   });

   // API payload validation
   app.useGlobalPipes(new ValidationPipe());

   // Global exception filter
   app.useGlobalFilters(new ExceptionFilter());

   if (onBeforeListen) {
      await Util.call(app, onBeforeListen);
   }

   // Wrap the application with the metadata
   metadata.setService(app);
   await app
      .listen()
      .then(() => console.log(`${queue} is listening`))
      .catch((e) => console.debug(`${queue} listen ERR`, e));
}
