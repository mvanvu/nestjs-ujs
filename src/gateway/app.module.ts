import { MiddlewareConsumer, Module, VersioningType } from '@nestjs/common';
import { APP_GUARD, NestFactory } from '@nestjs/core';
import { clientProxies } from './proxy.module';
import helmet from 'helmet';
import { NestExpressApplication } from '@nestjs/platform-express';
import * as path from 'path';
import { TransformInterceptor, ValidationPipe, ExceptionFilter } from '@lib/common';
import { DocumentBuilder, SwaggerModule } from '@nestjs/swagger';
import { HttpMiddleware } from './http.middleware';
import { UserAuthGuard, UserRoleGuard } from './lib';
import { bootstrap, appConfig } from '@metadata';
import { GroupController, RoleController, UserController } from './user/controller';
import { FileController } from './storage/controller';
import { FileProvider } from './storage/provider';

@Module({
   imports: [...clientProxies],
   controllers: [GroupController, RoleController, UserController, FileController],
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

      const staticPath = appConfig.get('storage.localPath');

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
