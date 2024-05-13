import { Registry, Util } from '@mvanvu/ujs';
import { Inject, Injectable } from '@nestjs/common';
import { REQUEST } from '@nestjs/core';
import { ClientProxy, RmqRecordBuilder } from '@nestjs/microservices';
import { HttpRequest, MessageData, ServiceOptions, ThrowException, metadata } from '@lib';
import { lastValueFrom, timeout } from 'rxjs';

export class BaseClientProxy {
   constructor(
      readonly client: ClientProxy,
      readonly req: HttpRequest,
   ) {}

   async send<TInput, TResult>(
      messagePattern: string,
      dataDelivery: MessageData<TInput>,
      options?: ServiceOptions,
   ): Promise<TResult> {
      try {
         const record = new RmqRecordBuilder<any>(dataDelivery.data || {})
            .setOptions({
               headers: {
                  'x-meta': Registry.from({})
                     .extends(dataDelivery.meta || {})
                     .extends({ headers: this.req.registry.valueOf() })
                     .toString(),
               },
            })
            .build();

         const response = await lastValueFrom(
            this.client.send(messagePattern, record).pipe(timeout(options?.timeOut ?? 5000)),
         );

         // Todo, handle response data
         return response;
      } catch (e) {
         Util.debug(e);
         new ThrowException(e);
      }
   }
}

@Injectable()
export class BaseController {
   @Inject(REQUEST) readonly req: HttpRequest;

   createClientProxy(proxy: string): BaseClientProxy {
      class InstClientProxy extends BaseClientProxy {}
      // Object.defineProperty(InstClientProxy, 'name', `Client${proxy}`);

      return new InstClientProxy(metadata.getGateway().get<ClientProxy>(proxy), this.req);
   }
}
