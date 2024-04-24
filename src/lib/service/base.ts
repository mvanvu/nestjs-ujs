import { ThrowException } from '@lib/exception';
import { metadata } from '../metadata';
import { DataDelivery, HttpRequest, MessageData, ServiceExecuteOptions, ServiceOptions } from '../type';
import { Is, Registry, Util } from '@mvanvu/ujs';
import { Inject, Injectable } from '@nestjs/common';
import { REQUEST } from '@nestjs/core';
import { ClientProxy } from '@nestjs/microservices';
import { lastValueFrom, timeout } from 'rxjs';

@Injectable()
export abstract class BaseService {
   abstract readonly options: ServiceOptions;

   @Inject(REQUEST) readonly req: HttpRequest;

   async execute<TInput, TResult>(
      messagePattern: string,
      data: MessageData<TInput> | TInput,
      options?: ServiceExecuteOptions,
   ): Promise<TResult> {
      if (metadata.isGateway()) {
         // Handle for the api gateway
         const dataDelivery: DataDelivery = {
            data: data ?? null,
            meta: { ...this.req.registry.valueOf(), query: this.req.query },
         };

         try {
            const app = metadata.getGateway();
            const client = app.get<ClientProxy>(this.options.config.proxy);
            const response = await lastValueFrom(
               client.send<TResult, DataDelivery>(messagePattern, dataDelivery).pipe(timeout(options?.timeOut ?? 5000)),
            );

            // Todo, handle response data
            return response;
         } catch (e) {
            new ThrowException(e);
         }
      } else {
         const method: string = messagePattern.split('.').pop();

         if (Is.callable(this[method])) {
            const dataDelivery = data as MessageData<TInput>;

            return Util.callAsync(this, this[method], {
               data: dataDelivery.data,
               meta: Registry.from(dataDelivery.meta),
            });
         }

         new ThrowException(`The method ${this.constructor.name}.${method} is not a function`);
      }
   }
}
