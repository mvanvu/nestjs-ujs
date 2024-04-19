declare namespace NodeJS {
   interface ProcessEnv {
      // Common
      NODE_ENV: 'development' | 'production' | 'test';
      APP_ENV: 'gateway' | 'user';
      RABBITMQ_URL: string;
      MULTILINGUAL?: boolean;
      ALLOW_LANGUAGE?: string;
      DEFAULT_LANGUAGE?: string;
      QUERY_PARAM_SCOPE?: string;
      LIST_ITEMS_PER_PAGE: number;
      LIST_ITEMS_MAX_LIMIT: number;
   }
}
