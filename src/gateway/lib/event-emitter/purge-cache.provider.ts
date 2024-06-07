import { Inject, Injectable } from '@nestjs/common';
import { OnEvent } from './event-emitter.decorator';
import { OnServiceResponse, eventConstant } from '@lib/common';
import { CACHE_MANAGER, Cache } from '@nestjs/cache-manager';
import { Is } from '@mvanvu/ujs';

@Injectable()
export class PurgeCacheProvider {
   @Inject(CACHE_MANAGER) private readonly cacheManager: Cache;

   @OnEvent(eventConstant.onServiceResponse)
   async purgeHttpCaching({ httpRequest }: OnServiceResponse): Promise<void> {
      // Check to purge HTTP caching
      if (httpRequest.method === 'GET') {
         return;
      }

      try {
         const promises = [];
         const requestUrlWithoutParams = httpRequest.url.replace(/\/?\?.*$/g, '');
         const keys = await this.cacheManager.store.keys();
         const reqUserId = httpRequest.registry.get('user.id');
         const { cacheRefKeys } = httpRequest;
         const isRelatedCacheKey = (cacheKey: string): boolean => {
            if (cacheRefKeys) {
               for (const refKey of Is.array(cacheRefKeys) ? cacheRefKeys : [cacheRefKeys]) {
                  if ((Is.string(refKey) && refKey === cacheKey) || (Is.regex(refKey) && refKey.test(cacheKey))) {
                     return true;
                  }
               }
            }

            return false;
         };

         for (const key of keys) {
            const [userId, cacheKey] = /^[0-9a-fA-F]{24}:/.test(key) ? key.split(':') : [null, key];
            const cacheKeyWithoutSuffix = cacheKey.replace(/\/?\?.*$/g, '');

            if (
               (userId && reqUserId === userId) ||
               cacheKey.startsWith(requestUrlWithoutParams) ||
               requestUrlWithoutParams.startsWith(cacheKeyWithoutSuffix) ||
               isRelatedCacheKey(cacheKey)
            ) {
               promises.push(
                  this.cacheManager.del(key).then(() => console.log(`Purge HTTP caching success, key: ${key}`)),
               );
            }
         }

         if (promises.length) {
            await Promise.all(promises);
         }
      } catch (e) {
         console.error(`Purge HTTP caching ERR:`, e);
      }
   }
}
