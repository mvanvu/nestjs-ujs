import { Registry } from '@mvanvu/ujs';
const envConfig = Registry.from(process.env);
const corsOrigin = envConfig.get<string>('CORS_ORIGIN', '*');
const appConfigData = {
   nodeEnv: envConfig.get<'development' | 'production' | 'test'>('NODE_ENV'),
   appEnv: envConfig.get<'api-gateway' | 'system' | 'user' | 'order' | 'content'>('APP_ENV'),
   multilingual: envConfig.get<boolean>('MULTILINGUAL', false, 'toBoolean'),
   defaultLanguage: envConfig.get<string>('DEFAULT_LANGUAGE', 'enGB'),
   acceptLanguage: envConfig.get<string>('ACCEPT_LANGUAGES', '*'),
   apiGateway: {
      port: envConfig.get<number>('APP_PORT', 9001, 'toNumber'),
      prefix: envConfig.get<string>('API_PREFIX', 'api'),
      cors: {
         enable: envConfig.get<boolean>('CORS_ENABLED', true, 'toBoolean'),
         origin: corsOrigin.includes(',') ? corsOrigin.split(',') : corsOrigin,
         methods: envConfig.get<string>('CORS_METHODS', '*').split(','),
      },
      throttler: envConfig
         .get<string>('THROTTLER_OPTIONS', 'short:1000:10,medium:10000:50,long:60000:100')
         .split(',')
         .map((opt) => {
            const [name, ttl, limit] = opt.split(':');
            return { name, ttl: Number(ttl), limit: Number(limit) };
         }),
      requestTimeout: envConfig.get<number>('HTTP_REQUEST_TIME_OUT', 5000, 'toNumber'),
   },
   redis: {
      url: envConfig.get<string>('REDIS_URL'),
   },
   cache: {
      ttl: envConfig.get<number>('CACHE_TTL_IN_SECONDS', 60, 'toNumber') * 1000,
      maxItems: envConfig.get<number>('CACHE_MAX_ITEMS', 5000, 'toNumber'),
   },
   rabbitMQ: {
      url: envConfig.get<string>('RABBITMQ_URL'),
   },
   language: {
      default: envConfig.get<string>('DEFAULT_LANGUAGE', 'en-GB'),
      accept: envConfig.get<string>('ACCEPT_LANGUAGE', '*').split(','),
   },
   rootUid: envConfig.get<string>('ROOT_UID', ''),
} as const;

export const appConfig = Registry.from<typeof appConfigData>(appConfigData, { consistent: true });
