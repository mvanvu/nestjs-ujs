import { Inject, Injectable } from '@nestjs/common';
import { OnServiceResponse, eventConstant } from '@shared-library';
import { CACHE_MANAGER, Cache } from '@nestjs/cache-manager';
import { Is } from '@mvanvu/ujs';
import { OnEvent } from '@gateway/@library/event-emitter.decorator';

@Injectable()
export class PurgeCacheProvider {
   @Inject(CACHE_MANAGER) private readonly cacheManager: Cache;

   @OnEvent(eventConstant.onServiceResponse)
   async purgeHttpCaching({ httpRequest, messagePattern, responseData, success }: OnServiceResponse): Promise<void> {
      // Check to purge HTTP caching
      if (httpRequest.method === 'GET') {
         return;
      }

      try {
         const activityLogsBaseKey = 'systems/activity-logs';
         const promises = [];
         const requestUrlWithoutParams = httpRequest.url.replace(/\/?\?.*$/g, '');
         const keys = await this.cacheManager.store.keys();
         const { cacheRefKeys } = httpRequest;
         const isRelatedCacheKey = (cacheKey: string): boolean => {
            if (cacheRefKeys) {
               for (const refKey of Is.array(cacheRefKeys) ? cacheRefKeys : [cacheRefKeys]) {
                  if (
                     (Is.string(refKey) && refKey === cacheKey) ||
                     (refKey instanceof RegExp && refKey.test(cacheKey))
                  ) {
                     return true;
                  }
               }
            }

            return false;
         };

         if (success) {
            for (const key of keys) {
               const [userId, cacheKey] = /^[0-9a-fA-F]{24}:/.test(key) ? key.split(':') : [null, key];
               const cacheKeyWithoutSuffix = cacheKey.replace(/\/?\?.*$/g, '');

               if (
                  (messagePattern.startsWith('user.') && responseData?.data?.id === userId) ||
                  cacheKey.startsWith(requestUrlWithoutParams) ||
                  requestUrlWithoutParams.startsWith(cacheKeyWithoutSuffix) ||
                  isRelatedCacheKey(cacheKey) ||
                  cacheKey.includes(activityLogsBaseKey)
               ) {
                  promises.push(
                     this.cacheManager.del(key).then(() => console.log(`Purge HTTP caching successfully, key: ${key}`)),
                  );
               }
            }
         } else {
            for (const key of keys) {
               if (key.includes(activityLogsBaseKey)) {
                  promises.push(
                     this.cacheManager.del(key).then(() => console.log(`Purge HTTP caching successfully, key: ${key}`)),
                  );
               }
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
