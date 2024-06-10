import { EventEmitter, Is, Registry } from '@mvanvu/ujs';
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
      dataDelivery?: MessageData<TInput>,
      options?: ServiceOptions,
   ): Promise<TResult> {
      const eventPayload: OnServiceResponse = {
         messagePattern,
         requestData: dataDelivery,
         httpRequest: this.req,
         success: true,
      };

      try {
         const record = new RmqRecordBuilder<any>(dataDelivery?.data || {})
            .setOptions({
               headers: {
                  'x-meta': Registry.from({})
                     .extends(dataDelivery?.meta || {})
                     .extends({ headers: this.req.registry.valueOf() })
                     .toString(),
               },
            })
            .build();

         const response = await lastValueFrom(
            this.client.send(messagePattern, record).pipe(timeout(options?.timeOut ?? 5000)),
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
         console.debug(e);

         // Emit the response failure
         eventPayload.success = false;
         eventPayload.responseData = e;

         try {
            await this.eventEmitter.emitAsync(eventConstant.onServiceResponse, eventPayload);
         } catch {}

         new ThrowException(e?.error || e);
      }
   }
}
