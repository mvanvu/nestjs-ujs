import { MiddlewareConsumer, Module, VersioningType } from '@nestjs/common';
import { APP_GUARD, APP_INTERCEPTOR, NestFactory } from '@nestjs/core';
import helmet from 'helmet';
import { NestExpressApplication } from '@nestjs/platform-express';
import * as path from 'path';
import { TransformInterceptor, ValidationPipe, ExceptionFilter } from '@lib/common';
import { DocumentBuilder, SwaggerModule } from '@nestjs/swagger';
import { EventEmitterModule, HttpCacheInterceptor, HttpMiddleware, UserAuthGuard, UserRoleGuard } from './lib';
import { bootstrap, appConfig, serviceListNames, serviceConfig } from '@metadata';
import { GroupController, RoleController, UserController } from './user/controller';
import { StorageController } from './storage/controller';
import { FileProvider } from './storage/provider';
import { redisStore } from 'cache-manager-redis-yet';
import { CacheModule } from '@nestjs/cache-manager';
import { ClientsModule, ClientsProviderAsyncOptions, Transport } from '@nestjs/microservices';
import { MailerService } from './mailer/provider';
import { SystemController } from './system/controller';
import { SystemService } from './system/provider';

const createClientAsyncOptions = (name: string): ClientsProviderAsyncOptions => {
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
};
@Module({
   imports: [
      ...serviceListNames.map((name) =>
         ClientsModule.registerAsync({ isGlobal: true, clients: [createClientAsyncOptions(name)] }),
      ),
      EventEmitterModule,
      CacheModule.register({
         isGlobal: true,
         store: redisStore,
         url: appConfig.get('redis.url'),
         ttl: appConfig.get('cache.ttl'),
         max: appConfig.get('cache.maxItems'),
      }),
   ],
   controllers: [SystemController, GroupController, RoleController, UserController, StorageController],
   providers: [
      {
         provide: APP_INTERCEPTOR,
         useClass: HttpCacheInterceptor,
      },
      {
         provide: APP_GUARD,
         useClass: UserAuthGuard,
      },
      {
         provide: APP_GUARD,
         useClass: UserRoleGuard,
      },
      FileProvider,
      MailerService,
      SystemService,
   ],
})
export class AppModule {
   configure(consumer: MiddlewareConsumer) {
      consumer.apply(HttpMiddleware).forRoutes('*');
   }

   static async bootstrap(): Promise<void> {
      const app = await NestFactory.create<NestExpressApplication>(AppModule, {});

      app.enableShutdownHooks();
      app.use(helmet());

      const staticPath = serviceConfig.get('storage.upload.localPath');

      if (staticPath) {
         const rootPath = path.join(process.cwd(), staticPath, 'public');
         app.useStaticAssets(rootPath, { prefix: '/', index: false });
      }

      const apiPrefix = appConfig.get('apiGateway.prefix');

      if (apiPrefix) {
         app.setGlobalPrefix(apiPrefix);
      }

      app.enableVersioning({ defaultVersion: '1', type: VersioningType.URI });

      if (appConfig.get('apiGateway.cors.enable')) {
         app.enableCors({
            origin: appConfig.get('apiGateway.cors.origin'),
            methods: appConfig.get('apiGateway.cors.methods'),
         });
      }

      app.useGlobalFilters(new ExceptionFilter());
      app.useGlobalPipes(new ValidationPipe());
      app.useGlobalInterceptors(new TransformInterceptor());

      const swaggerConfig = new DocumentBuilder()
         .setTitle('NestJS UJS')
         .setDescription('NestJS App API')
         .addBearerAuth()
         .build();
      const document = SwaggerModule.createDocument(app, swaggerConfig);
      SwaggerModule.setup('api-docs', app, document, { swaggerOptions: { persistAuthorization: true } });
      const port = appConfig.get('apiGateway.port');

      // Bootstrap Metadata
      bootstrap(app);

      await app
         .listen(port, () => console.log(`Listening on port: ${port}`))
         .catch((e) => console.error('Init app failure:', e));
   }
}
