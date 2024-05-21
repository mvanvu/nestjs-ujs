declare namespace NodeJS {
   interface ProcessEnv {
      // Common
      NODE_ENV: 'development' | 'production' | 'test';
      APP_ENV: 'gateway' | 'user' | 'storage';
   }
}
