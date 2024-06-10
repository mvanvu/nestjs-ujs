import { Registry } from '@mvanvu/ujs';
import dotenv from 'dotenv';
import dotenvExpand from 'dotenv-expand';
dotenvExpand.expand(dotenv.config());
const envConfig = Registry.from(process.env);

const appConfigData = {
   nodeEnv: envConfig.get<'development' | 'production' | 'test'>('NODE_ENV'),
   appEnv: envConfig.get<'gateway' | 'mailer' | 'storage' | 'user'>('APP_ENV'),
   apiGateway: {
      port: envConfig.get<number>('PORT', 9000, 'toUInt'),
      prefix: envConfig.get<string>('API_PREFIX', 'api'),
      cors: {
         enable: envConfig.get<boolean>('CORS_ENABLED', true, 'toBoolean'),
         origin: envConfig.get<string>('CORS_ORIGIN', '*').split(','),
         methods: envConfig.get<string>('CORS_METHODS', '*').split(','),
      },
   },
   redis: {
      url: envConfig.get<string>('REDIS_URL'),
   },
   cache: {
      ttl: envConfig.get<number>('CACHE_TTL_IN_SECONDS', 300, 'toUInt') * 1000,
      maxItems: envConfig.get<number>('CACHE_MAX_ITEMS', 1000, 'toUInt'),
   },
   rabbitMQ: {
      url: envConfig.get<string>('RABBITMQ_URL'),
   },
   language: {
      default: envConfig.get<string>('DEFAULT_LANGUAGE', 'en-GB'),
      accept: envConfig.get<string>('ACCEPT_LANGUAGE', '*').split(','),
   },
   queryScope: envConfig.get<string>('QUERY_PARAM_SCOPE', ''),
} as const;

type AppConfigData = typeof appConfigData;
export const appConfig = Registry.from<AppConfigData>(appConfigData, { consistent: true });
