import { ValidationPipe, ExceptionFilter, TransformInterceptor } from '@shared-library';
import { Callable, ClassConstructor, Util } from '@mvanvu/ujs';
import { NestFactory } from '@nestjs/core';
import { MicroserviceOptions, Transport } from '@nestjs/microservices';
import { appConfig, bootstrap } from '@metadata';

export async function createMicroserviceApp(
   AppModule: ClassConstructor<any>,
   serviceName: string,
   onBeforeListen?: Callable,
) {
   const app = await NestFactory.createMicroservice(AppModule, <MicroserviceOptions>{
      transport: Transport.RMQ,
      options: {
         urls: [appConfig.get('rabbitMQ.url')],
         queue: `${serviceName.toUpperCase()}_MICROSERVICE`,
         queueOptions: { durable: true },
      },
   });

   // API payload validation
   app.useGlobalPipes(new ValidationPipe());

   // Global exception filter
   app.useGlobalFilters(new ExceptionFilter());

   // Global transform response
   app.useGlobalInterceptors(new TransformInterceptor());

   if (onBeforeListen) {
      await Util.call(app, onBeforeListen);
   }

   // Bootstrap Metadata
   bootstrap(app);
   await app
      .listen()
      .then(() => console.log(`The ${serviceName} microservice is listening, NODE_ENV=${appConfig.get('nodeEnv')}`))
      .catch((e) => console.debug(`The ${serviceName} microservice listens ERR`, e));
}
