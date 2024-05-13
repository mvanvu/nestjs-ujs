import { MiddlewareConsumer, Module, VersioningType } from '@nestjs/common';
import { APP_GUARD, NestFactory } from '@nestjs/core';
import proxies from './proxy.module';
import helmet from 'helmet';
import { NestExpressApplication } from '@nestjs/platform-express';
import * as path from 'path';
import { HttpTransform, ValidationPipe, metadata, ExceptionFilter } from '@lib';
import { DocumentBuilder, SwaggerModule } from '@nestjs/swagger';
import { HttpMiddleware } from './http.middleware';
import { UserAuthGuard, UserRoleGuard } from '@/microservice/user/auth/guard';
import { appConfig } from '@config';
import { RoleController, UserController } from './controller';
import { FileProvider } from './provider';

@Module({
   imports: proxies,
   controllers: [RoleController, UserController],
   providers: [
      {
         provide: APP_GUARD,
         useClass: UserAuthGuard,
      },
      {
         provide: APP_GUARD,
         useClass: UserRoleGuard,
      },
      FileProvider,
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

      const staticPath = appConfig.storage.localPath;

      if (staticPath) {
         const rootPath = path.join(process.cwd(), staticPath, 'public');
         app.useStaticAssets(rootPath, { prefix: '/', index: false });
      }

      const apiPrefix = appConfig.apiGateway.prefix;

      if (apiPrefix) {
         app.setGlobalPrefix(apiPrefix);
      }

      app.enableVersioning({ defaultVersion: '1', type: VersioningType.URI });

      if (appConfig.apiGateway.cors.enable) {
         app.enableCors({ origin: appConfig.apiGateway.cors.origin, methods: appConfig.apiGateway.cors.methods });
      }

      app.useGlobalFilters(new ExceptionFilter());
      app.useGlobalPipes(new ValidationPipe());
      app.useGlobalInterceptors(new HttpTransform());

      const swaggerConfig = new DocumentBuilder()
         .setTitle('NestJS UJS')
         .setDescription('NestJS App API')
         .addBearerAuth()
         .build();
      const document = SwaggerModule.createDocument(app, swaggerConfig);
      SwaggerModule.setup('api-docs', app, document, { swaggerOptions: { persistAuthorization: true } });
      const port = appConfig.apiGateway.port;

      // Metadata
      metadata.setGateway(app);

      await app
         .listen(port, () => console.log(`Listening on port: ${port}`))
         .catch((e) => console.error('Init app failure:', e));
   }
}
