import { Inject, Injectable } from '@nestjs/common';
import { REQUEST } from '@nestjs/core';
import { ClientProxy } from '@nestjs/microservices';
import { HttpRequest } from '@lib/common';
import { app } from '@metadata';
import { BaseClientProxy } from './client-proxy.base';
import { EventEmitter } from '@mvanvu/ujs';

@Injectable()
export class BaseController {
   @Inject(REQUEST) private readonly req: HttpRequest;

   @Inject(EventEmitter)
   private readonly eventEmitter: EventEmitter;

   createClientProxy(serviceName: string): BaseClientProxy {
      const proxyName = serviceName.toUpperCase() + '_MICROSERVICE';
      class InstClientProxy extends BaseClientProxy {}
      Object.defineProperty(InstClientProxy, 'name', { value: proxyName });

      return new InstClientProxy(app().get<ClientProxy>(proxyName), this.req, this.eventEmitter);
   }
}
