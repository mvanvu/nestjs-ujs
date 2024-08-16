import { appConfig } from './config';

async function bootstrap() {
   const appEnv = appConfig.get('appEnv');
   const appModulePath = `./${appEnv === 'api-gateway' ? appEnv : `microservice/${appEnv}`}/app.module`;

   // Dynamic import application
   const { AppModule } = await import(appModulePath);
   await AppModule.bootstrap();
}

bootstrap().catch(console.error);
