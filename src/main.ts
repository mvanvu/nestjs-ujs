import { appConfig } from './config';

async function bootstrap() {
   switch (appConfig.get('appEnv')) {
      case 'gateway':
         import('./gateway/app.module').then(({ AppModule }) => AppModule.bootstrap());
         break;

      case 'user':
         import('./microservice/user/app.module').then(({ AppModule }) => AppModule.bootstrap());
         break;

      case 'storage':
         import('./microservice/storage/app.module').then(({ AppModule }) => AppModule.bootstrap());
         break;
   }
}

bootstrap().catch(console.debug);
