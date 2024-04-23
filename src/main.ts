async function bootstrap() {
   const appEnv = process.env.APP_ENV;

   switch (appEnv) {
      case 'gateway':
         return import('./lib/core/http.gateway').then(({ AppModule }) => AppModule.bootstrap());

      case 'user':
         return import('./lib/core/rpc.user').then(({ AppModule }) => AppModule.bootstrap());
   }
}

bootstrap();
