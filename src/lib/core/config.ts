import { Registry } from '@mvanvu/ujs';
import dotenv from 'dotenv';
import dotenvExpand from 'dotenv-expand';
dotenvExpand.expand(dotenv.config());

export const envConfig = Registry.from(process.env);
const appConfigData = {
   nodeEnv: envConfig.get<'development' | 'production' | 'test'>('NODE_ENV'),
   appEnv: envConfig.get<'gateway' | 'user'>('APP_ENV'),
   apiGateway: {
      port: envConfig.get<number>('PORT', 9000, 'toUInt'),
      prefix: envConfig.get<string>('API_PREFIX', 'api'),
      cors: {
         enable: envConfig.get<boolean>('CORS_ENABLED', true, 'toBoolean'),
         origin: envConfig.get<string>('CORS_ORIGIN', '*').split(','),
         methods: envConfig.get<string>('CORS_METHODS', '*').split(','),
      },
   },
   storage: {
      localPath: envConfig.get<string | undefined>('MEDIA_STORAGE_LOCAL_PATH'),
   },
   jwt: {
      secret: envConfig.get<string>('JWT_SECRET_KEY'),
      accessExpiresInMinutes: envConfig.get<number>('JWT_ACCESS_EXPIRES_IN_MINUTES', 60, 'UInt'),
      refreshExpiresInMinutes: envConfig.get<number>('JWT_REFRESH_EXPIRES_IN_MINUTES', 70, 'UInt'),
   },
   redis: {
      url: envConfig.get<string>('REDIS_URL'),
   },
   rabbitMQ: {
      url: envConfig.get<string>('RABBITMQ_URL'),
   },
   mongodb: {
      userLiveUrl: envConfig.get<string>('MONGODB_USER_URL'),
      userTestUrl: envConfig.get<string>('MONGODB_USER_TEST_URL'),
   },
   language: {
      default: envConfig.get<string>('DEFAULT_LANGUAGE', 'en-GB'),
      accept: envConfig.get<string>('ACCEPT_LANGUAGE', '*').split(','),
   },
   list: {
      scope: envConfig.get<string>('QUERY_PARAM_SCOPE', ''),
      limit: envConfig.get<number>('LIST_ITEMS_PER_PAGE', 25, 'toUInt'),
      maxLimit: envConfig.get<number>('LIST_ITEMS_MAX_LIMIT', 1000, 'toUInt'),
   },
   rootUid: envConfig.get<string>('ROOT_UID'),
};

export type AppConfigData = typeof appConfigData;
export const appConfig: AppConfigData = appConfigData;
