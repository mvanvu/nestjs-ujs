// import { UserGuard } from '@lib/interceptor';
import { UserModule } from '@/service/user/user.module';
import { Module, VersioningType } from '@nestjs/common';
import { ConfigModule, ConfigService } from '@nestjs/config';
import { APP_GUARD, NestFactory } from '@nestjs/core';
import proxies from './proxy.module';
import helmet from 'helmet';
import { NestExpressApplication } from '@nestjs/platform-express';
import * as path from 'path';
import { UserGuard, ValidationPipe, metadata } from '@lib';
import { DocumentBuilder, SwaggerModule } from '@nestjs/swagger';

@Module({
   imports: [
      ConfigModule.forRoot({ isGlobal: true, expandVariables: true }),
      ...proxies,
      // Service modules
      UserModule,
   ],
   providers: [
      {
         provide: APP_GUARD,
         useClass: UserGuard,
      },
   ],
})
export class AppModule {
   static async bootstrap(): Promise<void> {
      const app = await NestFactory.create<NestExpressApplication>(AppModule, {});
      const config = app.get(ConfigService);

      app.enableShutdownHooks();
      app.use(helmet());

      const staticPath = config.get('MEDIA_STORAGE_LOCAL_PATH');

      if (staticPath) {
         const rootPath = path.join(process.cwd(), staticPath, 'public');
         app.useStaticAssets(rootPath, { prefix: '/', index: false });
      }

      const apiPrefix = config.get('API_PREFIX');

      if (apiPrefix) {
         app.setGlobalPrefix(apiPrefix);
      }

      app.enableVersioning({ defaultVersion: '1', type: VersioningType.URI });

      if (config.get('CORS_ENABLED') === 'true') {
         const origin = config.get<string>('CORS_ORIGIN') || '*';
         const methods = config.get<string>('CORS_METHODS') || undefined;
         app.enableCors({ origin: origin.includes(',') ? origin.split(',') : origin, methods });
      }

      app.useGlobalPipes(new ValidationPipe());
      const swaggerConfig = new DocumentBuilder()
         .setTitle('Nexle')
         .setDescription('Document API')
         .addBearerAuth()
         .build();
      const document = SwaggerModule.createDocument(app, swaggerConfig);
      SwaggerModule.setup('api-docs', app, document, { swaggerOptions: { persistAuthorization: true } });
      const port = config.get('PORT');

      // Metadata
      metadata.setGateway(app);

      await app
         .listen(port, () => console.log(`Listening on port: ${port}`))
         .catch((e) => console.error('Init app failure:', e));
   }
}
