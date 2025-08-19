import { Module } from '@nestjs/common';
import { CacheModule as NestCacheModule } from '@nestjs/cache-manager';
import Keyv from 'keyv';
import KeyvRedis from '@keyv/redis';
import { appConfig } from '@metadata';
import { CacheService } from './cache.service';

@Module({
   imports: [
      NestCacheModule.registerAsync({
         isGlobal: true,

         useFactory: async () => {
            const redis = new Keyv({
               namespace: 'cache',
               store: new KeyvRedis(appConfig.get('redis.url')),
               ttl: appConfig.get('cache.ttl'),
            });

            return { stores: [redis] };
         },
      }),
   ],
   providers: [CacheService],
   exports: [CacheService],
})
export class CacheModule {}
