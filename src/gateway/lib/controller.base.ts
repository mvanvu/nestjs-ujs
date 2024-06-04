import { Inject, Injectable } from '@nestjs/common';
import { REQUEST } from '@nestjs/core';
import { ClientProxy } from '@nestjs/microservices';
import { HttpRequest, eventConstant } from '@lib/common';
import { app } from '@metadata';
import { CACHE_MANAGER, Cache } from '@nestjs/cache-manager';
import { OnEvent } from './event-emitter';
import { BaseClientProxy } from './client-proxy.base';

@Injectable()
export class BaseController {
   @Inject(CACHE_MANAGER) private readonly cacheManager: Cache;

   @Inject(REQUEST) private readonly req: HttpRequest;

   createClientProxy(serviceName: string): BaseClientProxy {
      const proxyName = serviceName.toUpperCase() + '_MICROSERVICE';
      class InstClientProxy extends BaseClientProxy {}
      Object.defineProperty(InstClientProxy, 'name', { value: proxyName });

      return new InstClientProxy(app().get<ClientProxy>(proxyName), this.req);
   }

   @OnEvent(eventConstant.onServiceResponse)
   async purgeHttpCaching(): Promise<void> {
      // Check to purge HTTP caching
      if (this.req.method === 'GET') {
         return;
      }

      try {
         const promises = [];
         const requestUrlWithoutParams = this.req.url.replace(/\/?\?.*$/g, '');
         const keys = await this.cacheManager.store.keys();
         const reqUserId = this.req.registry.get('user.id');

         for (const key of keys) {
            const [userId, cacheKey] = /^[0-9a-fA-F]{24}:/.test(key) ? key.split(':') : [null, key];
            const cacheKeyWithoutSuffix = cacheKey.replace(/\/?\?.*$/g, '');

            if (
               (reqUserId && reqUserId === userId) ||
               cacheKey.startsWith(requestUrlWithoutParams) ||
               requestUrlWithoutParams.startsWith(cacheKeyWithoutSuffix)
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
