import { Inject, Injectable } from '@nestjs/common';
import { REQUEST } from '@nestjs/core';
import { ClientProxy } from '@nestjs/microservices';
import { HttpRequest } from '@lib/common';
import { app } from '@metadata';
import { BaseClientProxy } from './client-proxy.base';
import { EventEmitter } from '@mvanvu/ujs';
import { CACHE_MANAGER, Cache } from '@nestjs/cache-manager';

@Injectable()
export class BaseController {
   @Inject(REQUEST) protected readonly req: HttpRequest;

   @Inject(EventEmitter)
   protected readonly eventEmitter: EventEmitter;

   @Inject(CACHE_MANAGER) protected readonly cacheManager: Cache;

   createClientProxy(serviceName: string): BaseClientProxy {
      const proxyName = serviceName.toUpperCase() + '_MICROSERVICE';
      class InstClientProxy extends BaseClientProxy {}
      Object.defineProperty(InstClientProxy, 'name', { value: proxyName });

      return new InstClientProxy(app().get<ClientProxy>(proxyName), this.req, this.eventEmitter);
   }
}
