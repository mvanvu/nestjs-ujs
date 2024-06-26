import { DynamicModule, Global, Module, VersioningType } from '@nestjs/common';
import { APP_GUARD, APP_INTERCEPTOR, DiscoveryModule, NestFactory, REQUEST } from '@nestjs/core';
import helmet from 'helmet';
import { NestExpressApplication } from '@nestjs/platform-express';
import * as path from 'path';
import { ValidationPipe, ExceptionFilter, TransformInterceptor } from '@shared-library';
import { DocumentBuilder, SwaggerModule } from '@nestjs/swagger';
import { BaseClientProxy, EventEmitterLoader, HttpCacheInterceptor, UserAuthGuard, UserRoleGuard } from './@library';
import { bootstrap, appConfig, serviceListNames, serviceConfig } from '@metadata';
import { redisStore } from 'cache-manager-redis-yet';
import { CacheModule } from '@nestjs/cache-manager';
import { ClientsModule, Transport } from '@nestjs/microservices';
import { EventEmitter, Util } from '@mvanvu/ujs';
import { ThrottlerGuard, ThrottlerModule } from '@nestjs/throttler';
import { Language } from '@shared-library';
import { Request } from 'express';

@Global()
@Module({
   imports: [DiscoveryModule],
   providers: [
      BaseClientProxy,
      EventEmitterLoader,
      {
         provide: EventEmitter,
         useValue: new EventEmitter(),
      },
      {
         provide: Language,
         useFactory: (req: Request) => {
            let lang: string = (req.query?.lang || req.headers['x-language']) as string;

            const acceptLanguage = appConfig.get('acceptLanguage');

            if (!lang || !acceptLanguage.split(/\s*[,|]\s*/).includes(lang)) {
               lang = appConfig.get('defaultLanguage', 'en-GB');
            }

            return new Language(lang);
         },
         inject: [REQUEST],
      },
   ],
   exports: [EventEmitter, BaseClientProxy, Language],
})
class CoreModule {}

@Module({})
class MicroserviceModule {
   static registerAsync(): DynamicModule {
      return {
         module: CoreModule,
         imports: serviceListNames.map((serviceName) =>
            import(`./${serviceName}/${serviceName}.module`).then(
               (moduleRef) => moduleRef[`${Util.uFirst(serviceName)}Module`],
            ),
         ),
      };
   }
}

@Module({
   imports: [
      ThrottlerModule.forRoot(appConfig.get('apiGateway.throttler')),
      ...serviceListNames.map((name) =>
         ClientsModule.registerAsync({
            isGlobal: true,
            clients: [
               {
                  name: `${name.toUpperCase()}_MICROSERVICE`,
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
               },
            ],
         }),
      ),
      CacheModule.register({
         isGlobal: true,
         store: redisStore,
         url: appConfig.get('redis.url'),
         ttl: appConfig.get('cache.ttl'),
         max: appConfig.get('cache.maxItems'),
      }),
      // Dynamic microservice modules
      MicroserviceModule.registerAsync(),
   ],
   providers: [
      {
         provide: APP_INTERCEPTOR,
         useClass: HttpCacheInterceptor,
      },
      {
         provide: APP_GUARD,
         useClass: ThrottlerGuard,
      },
      {
         provide: APP_GUARD,
         useClass: UserAuthGuard,
      },
      {
         provide: APP_GUARD,
         useClass: UserRoleGuard,
      },
   ],
})
export class AppModule {
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

      if (!appConfig.is('appEnv', 'production')) {
         const swaggerConfig = new DocumentBuilder()
            .setTitle('NestJS UJS')
            .setDescription('NestJS App API')
            .addBearerAuth()
            .build();
         const document = SwaggerModule.createDocument(app, swaggerConfig);
         SwaggerModule.setup('api-docs', app, document, {
            swaggerOptions: { persistAuthorization: true, defaultModelsExpandDepth: -1 },
         });
      }

      const port = appConfig.get('apiGateway.port');

      // Bootstrap Metadata
      bootstrap(app);
      await app
         .listen(port, () => console.log(`Listening on port: ${port}, NODE_ENV=${appConfig.get('nodeEnv')}`))
         .catch((e) => console.error('Init app failure:', e));
   }
}
