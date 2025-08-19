import { Inject, Injectable } from '@nestjs/common';
import { CacheService, OnServiceResponse, eventConstant } from '@shared-library';
import { Is } from '@mvanvu/ujs';
import { OnEvent } from '@gateway/@library/event-emitter.decorator';

@Injectable()
export class PurgeCacheProvider {
   @Inject(CacheService) private readonly cacheService: CacheService;

   @OnEvent(eventConstant.onServiceResponse)
   async purgeHttpCaching({ httpRequest, success }: OnServiceResponse): Promise<void> {
      // Check to purge HTTP caching
      if (httpRequest.method === 'GET') {
         return;
      }

      try {
         const promises: Promise<void>[] = [];
         const activityLogsBaseKey = 'systems/activity-logs';
         const requestUrlWithoutParams = httpRequest.url.replace(/\/?\?.*$/g, '');
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

         const keys = await this.cacheService.getKeys();

         if (success) {
            for (const key of keys) {
               const [, cacheKey] = /^[0-9a-fA-F]{24}:/.test(key) ? key.split(':') : [null, key];
               const replaceReg = /^\/api\/v\d+\/|\??.*$/g;
               const rootKeyPath = cacheKey.replace(replaceReg, '');
               const rootEndpointPath = requestUrlWithoutParams.replace(replaceReg, '');

               if (
                  rootKeyPath === rootEndpointPath ||
                  isRelatedCacheKey(cacheKey) ||
                  cacheKey.includes(activityLogsBaseKey)
               ) {
                  promises.push(
                     this.cacheService
                        .delete(key)
                        .then(() => console.log(`Purge HTTP caching successfully, key: ${key}`)),
                  );
               }
            }
         } else {
            for (const key of keys) {
               if (key.includes(activityLogsBaseKey)) {
                  promises.push(
                     this.cacheService
                        .delete(key)
                        .then(() => console.log(`Purge HTTP caching successfully, key: ${key}`)),
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
