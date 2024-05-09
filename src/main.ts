import { appConfig } from './config';

async function bootstrap() {
   switch (appConfig.appEnv) {
      case 'gateway':
         return import('./gateway/app.module').then(({ AppModule }) => AppModule.bootstrap());

      case 'user':
         return import('./microservice/user/app.module').then(({ AppModule }) => AppModule.bootstrap());

      case 'storage':
         return import('./microservice/storage/app.module').then(({ AppModule }) => AppModule.bootstrap());
   }
}

bootstrap();
