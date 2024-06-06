import { EventEmitter, Registry, Util } from '@mvanvu/ujs';
import { Injectable } from '@nestjs/common';
import { ClientProxy, RmqRecordBuilder } from '@nestjs/microservices';
import {
   HttpRequest,
   MessageData,
   OnServiceResponse,
   ServiceOptions,
   ThrowException,
   eventConstant,
} from '@lib/common';
import { lastValueFrom, timeout } from 'rxjs';

@Injectable()
export class BaseClientProxy {
   constructor(
      private readonly client: ClientProxy,
      private readonly req: HttpRequest,
      private readonly eventEmitter: EventEmitter,
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

         // Emit an event
         const eventPayload: OnServiceResponse = {
            messagePattern,
            requestData: dataDelivery,
            responseData: response,
            httpRequest: this.req,
         };
         await this.eventEmitter.emitAsync(eventConstant.onServiceResponse, eventPayload);

         return response;
      } catch (e: any) {
         Util.debug(e);
         new ThrowException(e?.error || e);
      }
   }
}
