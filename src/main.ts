async function bootstrap() {
   const appEnv = process.env.APP_ENV;

   switch (appEnv) {
      case 'gateway':
         return import('./lib/app/http.gateway').then(({ AppModule }) => AppModule.bootstrap());

      case 'user':
         return import('./lib/app/rpc.user').then(({ AppModule }) => AppModule.bootstrap());
   }
}

bootstrap();
