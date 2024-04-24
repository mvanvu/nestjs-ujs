import { appConfig } from '@lib';

async function bootstrap() {
   switch (appConfig.appEnv) {
      case 'gateway':
         return import('./lib/core/gateway/app.module').then(({ AppModule }) => AppModule.bootstrap());

      case 'user':
         return import('./lib/core/service/user/app.module').then(({ AppModule }) => AppModule.bootstrap());
   }
}

bootstrap();
