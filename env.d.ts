declare namespace NodeJS {
   interface ProcessEnv {
      // Common
      NODE_ENV: 'development' | 'production' | 'test';
      APP_ENV: 'gateway' | 'user';
      RABBITMQ_URL: string;
      MULTILINGUAL?: 'true' | 'false';
      ALLOW_LANGUAGE?: string;
      DEFAULT_LANGUAGE?: string;
      QUERY_PARAM_SCOPE?: string;
   }
}
