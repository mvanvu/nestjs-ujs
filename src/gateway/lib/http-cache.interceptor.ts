import { CACHE_MANAGER, Cache, CacheInterceptor } from '@nestjs/cache-manager';
import { ExecutionContext, Inject, Injectable } from '@nestjs/common';
import { CACHE_WITH_PREFIX_USER_ID_KEY, HttpRequest, NO_CACHE_KEY } from '@lib/common';

@Injectable()
export class HttpCacheInterceptor extends CacheInterceptor {
   @Inject(CACHE_MANAGER) readonly cacheManager: Cache;

   trackBy(context: ExecutionContext): string | undefined {
      const request = context.switchToHttp().getRequest<HttpRequest>();
      const ignoreCaching = this.reflector.getAllAndOverride(NO_CACHE_KEY, [context.getClass(), context.getHandler()]);
      const { httpAdapter } = this.httpAdapterHost;
      const isGetRequest = httpAdapter.getRequestMethod(request) === 'GET';
      const withPrefixUserId = this.reflector.getAllAndOverride(CACHE_WITH_PREFIX_USER_ID_KEY, [
         context.getClass(),
         context.getHandler(),
      ]);
      const requestUrl = httpAdapter.getRequestUrl(request);
      let cacheKey = requestUrl;

      if (withPrefixUserId && request.registry.has('user.id')) {
         cacheKey = `${request.registry.get('user.id')}:${cacheKey}`;
      }

      if (!isGetRequest || ignoreCaching) {
         if (!isGetRequest) {
            setTimeout(async () => {
               const requestUrlWithoutParams = requestUrl.replace(/\/?\?.*$/g, '');
               const promises = [];
               const keys = await this.cacheManager.store.keys();
               for (const key of keys) {
                  const cacheKey = key.includes(':') ? key.split(':')[1] : key;
                  const cacheKeyWithoutSuffix = cacheKey.replace(/\/?\?.*$/g, '');

                  if (
                     cacheKey.startsWith(requestUrlWithoutParams) ||
                     requestUrlWithoutParams.startsWith(cacheKeyWithoutSuffix)
                  ) {
                     promises.push(this.cacheManager.del(key));
                  }
               }

               if (promises.length) {
                  await Promise.allSettled(promises);
               }
            }, 0);
         }

         return undefined;
      }

      return cacheKey;
   }
}
