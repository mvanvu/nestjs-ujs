import { CACHE_MANAGER, Cache, CacheInterceptor } from '@nestjs/cache-manager';
import { ExecutionContext, Inject, Injectable } from '@nestjs/common';
import { HTTP_CACHE_KEY, HttpRequest, HttpCacheOptions } from '@lib';

@Injectable()
export class HttpCacheInterceptor extends CacheInterceptor {
   @Inject(CACHE_MANAGER) readonly cacheManager: Cache;

   trackBy(context: ExecutionContext): string | undefined {
      const request = context.switchToHttp().getRequest<HttpRequest>();
      const cacheMetadata = this.reflector.getAllAndOverride<HttpCacheOptions>(HTTP_CACHE_KEY, [
         context.getClass(),
         context.getHandler(),
      ]);

      const { httpAdapter } = this.httpAdapterHost;
      const isGetRequest = httpAdapter.getRequestMethod(request) === 'GET';
      const isHttpApp = httpAdapter && !!httpAdapter.getRequestMethod;

      if (isHttpApp && cacheMetadata?.cacheRefKeys) {
         request.cacheRefKeys = cacheMetadata.cacheRefKeys;
      }

      if (!isHttpApp || !isGetRequest || cacheMetadata?.disabled) {
         return undefined;
      }

      const cacheKeybase = cacheMetadata?.cacheKey ?? httpAdapter.getRequestUrl(request);
      const reqUserId = request.registry.get('user.id');
      let cacheKey = cacheKeybase;

      if (cacheMetadata?.withUserIdPrefix && reqUserId) {
         cacheKey = `${reqUserId}:${cacheKey}`;
      }

      return cacheKey;
   }
}
