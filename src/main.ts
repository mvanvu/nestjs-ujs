import { appConfig } from './config';

async function bootstrap() {
   const appEnv = appConfig.get('appEnv');
   const allApps: string[] = appEnv !== 'all' ? [appEnv] : ['gateway', 'user', 'storage'];

   for (const app of allApps) {
      switch (app) {
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
}

bootstrap().catch(console.debug);
