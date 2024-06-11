import { EventEmitter, Is, Registry } from '@mvanvu/ujs';
import { Inject, Injectable, NotImplementedException } from '@nestjs/common';
import { ClientProxy, RmqRecordBuilder } from '@nestjs/microservices';
import { HttpRequest, MessageData, OnServiceResponse, ServiceOptions, eventConstant } from '@lib/common';
import { lastValueFrom, timeout } from 'rxjs';
import { REQUEST } from '@nestjs/core';
import { ServiceName, appConfig, clientProxy } from '@metadata';

@Injectable()
export class BaseClientProxy {
   @Inject(REQUEST) private req: HttpRequest;

   @Inject(EventEmitter) private eventEmitter: EventEmitter;

   private clientProxy: ClientProxy;

   create(serviceName: ServiceName): BaseClientProxy {
      const instance = new BaseClientProxy();
      instance.req = this.req;
      instance.eventEmitter = this.eventEmitter;
      instance.clientProxy = clientProxy(serviceName);

      return instance;
   }

   async send<TResult, TInput>(
      messagePattern: string,
      dataDelivery?: MessageData<TInput>,
      options?: ServiceOptions,
   ): Promise<TResult> {
      if (!this.clientProxy) {
         throw new NotImplementedException('The client proxy is not initialized');
      }

      const eventPayload: OnServiceResponse = {
         messagePattern,
         requestData: dataDelivery?.data,
         httpRequest: this.req,
         success: true,
      };

      try {
         const regReq = this.req.registry.clone();
         const user = regReq.get('user');

         if (user) {
            // Just send some important user data
            regReq.set('user', { id: user.id, username: user.name, email: user.email, group: user.group });
         }

         const record = new RmqRecordBuilder<any>(dataDelivery?.data || {})
            .setOptions({
               headers: {
                  'x-meta': Registry.from({})
                     .extends(dataDelivery?.meta || {})
                     .extends({ headers: regReq.valueOf() })
                     .toString(),
               },
            })
            .build();

         const response = await lastValueFrom(
            this.clientProxy.send(messagePattern, record).pipe(timeout(options?.timeOut ?? 5000)),
         );

         // Emit the response success event
         eventPayload.responseData = response;
         await this.eventEmitter.emitAsync(eventConstant.onServiceResponse, eventPayload);

         // Check to remove unneeded metadata
         if (Is.object(response) && Is.object(response.meta)) {
            const allowedMeta: string[] = ['totalCount', 'page', 'limit'];

            for (const key in response.meta) {
               if (!allowedMeta.includes(key)) {
                  delete response.meta[key];
               }
            }
         }

         return response;
      } catch (e: any) {
         if (appConfig.is('nodeEnv', 'development')) {
            console.debug(e);
         }

         // Emit the response failure
         eventPayload.success = false;
         eventPayload.responseData = e;

         try {
            await this.eventEmitter.emitAsync(eventConstant.onServiceResponse, eventPayload);
         } catch {}

         throw e;
      }
   }
}
