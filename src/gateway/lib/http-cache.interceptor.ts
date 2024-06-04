import { CACHE_MANAGER, Cache, CacheInterceptor } from '@nestjs/cache-manager';
import { ExecutionContext, Inject, Injectable } from '@nestjs/common';
import { HTTP_CACHE_KEY, HttpRequest, ICacheOptions } from '@lib/common';

@Injectable()
export class HttpCacheInterceptor extends CacheInterceptor {
   @Inject(CACHE_MANAGER) readonly cacheManager: Cache;

   trackBy(context: ExecutionContext): string | undefined {
      const request = context.switchToHttp().getRequest<HttpRequest>();
      const cacheMetadata = this.reflector.getAllAndOverride<ICacheOptions>(HTTP_CACHE_KEY, [
         context.getClass(),
         context.getHandler(),
      ]);
      const { httpAdapter } = this.httpAdapterHost;
      const isGetRequest = httpAdapter.getRequestMethod(request) === 'GET';
      const isHttpApp = httpAdapter && !!httpAdapter.getRequestMethod;

      if (!isHttpApp || !isGetRequest || cacheMetadata?.disabled) {
         return undefined;
      }

      const requestUrl = httpAdapter.getRequestUrl(request);
      const reqUserId = request.registry.get('user.id');
      let cacheKey = requestUrl;

      if (cacheMetadata?.withUserIdPrefix && reqUserId) {
         cacheKey = `${reqUserId}:${cacheKey}`;
      }

      return cacheKey;
   }
}
