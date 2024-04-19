import { metadata } from '@/core/metadata';
import { ValidationPipe } from '@lib/pipe';
import { Callable, Util } from '@mvanvu/ujs';
import { NestFactory } from '@nestjs/core';
import { MicroserviceOptions, Transport } from '@nestjs/microservices';
import { ClassConstructor } from 'class-transformer';

export async function createMicroserviceApp(
   AppModule: ClassConstructor<any>,
   queue: string,
   onBeforeListen?: Callable,
) {
   const app = await NestFactory.createMicroservice(AppModule, <MicroserviceOptions>{
      transport: Transport.RMQ,
      options: { urls: [process.env.RABBITMQ_URL], queue, queueOptions: { durable: true } },
   });

   // API payload validation
   app.useGlobalPipes(new ValidationPipe());

   if (onBeforeListen) {
      await Util.call(app, onBeforeListen);
   }

   // Wrap the application with the metadata
   metadata.setService(app);

   app.listen().then(() => `${queue} is listening`);
}
