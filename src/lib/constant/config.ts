import { Registry } from '@mvanvu/ujs';

export const processEnv = Registry.from(process.env);
export const config = {
   nodeEnv: processEnv.get<'development' | 'production' | 'test'>('NODE_ENV'),
   appEnv: processEnv.get<'gateway' | 'user'>('APP_ENV'),
   apiGateway: {
      port: processEnv.get<number>('PORT', 9000, 'toUInt'),
      prefix: processEnv.get<string>('API_PREFIX', 'api'),
      cors: {
         enable: processEnv.get<boolean>('CORS_ENABLED', true, 'toBoolean'),
         origin: processEnv.get<string>('CORS_ORIGIN', '*').split(','),
         methods: processEnv.get<string>('CORS_METHODS', '*').split(','),
      },
   },
   Jwt: {
      secret: processEnv.get<string>('JWT_SECRET_KEY'),
      accessExpiresIn: processEnv.get<string>('JWT_ACCESS_EXPIRES_IN', '1h'),
      refreshExpiresIn: processEnv.get<string>('JWT_REFRESH_EXPIRES_IN', '1.2h'),
   },
   redis: {
      url: processEnv.get<string>('REDIS_URL'),
   },
   rabbitMQ: {
      url: processEnv.get<string>('RABBITMQ_URL'),
   },
   mongodb: {
      userLiveUrl: processEnv.get<string>('MONGODB_USER_URL'),
      userTestUrl: processEnv.get<string>('MONGODB_USER_TEST_URL'),
   },
   language: {
      default: processEnv.get<string>('DEFAULT_LANGUAGE', 'en-GB'),
      accept: processEnv.get<string>('ACCEPT_LANGUAGE', '*').split(','),
   },
   list: {
      scope: processEnv.get<string>('QUERY_PARAM_SCOPE', ''),
      limit: processEnv.get<number>('LIST_ITEMS_PER_PAGE', 25, 'toUInt'),
      maxLimit: processEnv.get<number>('LIST_ITEMS_MAX_LIMIT', 1000, 'toUInt'),
   },
};
