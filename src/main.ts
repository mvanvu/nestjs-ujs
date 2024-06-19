import { appConfig } from './config';

async function bootstrap() {
   const appEnv = appConfig.get('appEnv');
   const appModule = `./${appEnv === 'api-gateway' ? appEnv : `microservice/${appEnv}`}/app.module`;

   // Dynamic import application
   import(appModule).then(({ AppModule }) => AppModule.bootstrap());
}

bootstrap().catch(console.debug);
