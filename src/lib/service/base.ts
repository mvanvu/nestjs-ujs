import { metadata } from '../metadata';
import { HttpRequest, ServiceExecuteOptions, ServiceOptions } from '../type';
import { Is, Util } from '@mvanvu/ujs';
import { HttpException, HttpStatus, Inject, Injectable } from '@nestjs/common';
import { REQUEST } from '@nestjs/core';
import { ClientProxy, RmqRecordBuilder, RpcException } from '@nestjs/microservices';
import { lastValueFrom, timeout } from 'rxjs';

@Injectable()
export abstract class BaseService {
   abstract readonly options: ServiceOptions;

   @Inject(REQUEST) readonly req: HttpRequest;

   async execute<TInput, TResult>(
      messagePattern: string,
      data?: TInput,
      options?: ServiceExecuteOptions,
   ): Promise<TResult> {
      if (metadata.isGateway()) {
         // Handle for the api gateway
         try {
            const clientProxy = this.options.constant.proxy;
            const app = metadata.getGateway();
            const client = app.get<ClientProxy>(clientProxy);
            const record = new RmqRecordBuilder(data ?? null)
               .setOptions({
                  headers: {},
               })
               .build();
            const response = await lastValueFrom(
               client.send(messagePattern, record).pipe(timeout(options?.timeOut ?? 5000)),
            );

            // Todo, handle response data
            return response;
         } catch (e) {
            throw new HttpException(e, HttpStatus.BAD_GATEWAY);
         }
      } else {
         const method: string = messagePattern.split('.').pop();

         if (Is.callable(this[method])) {
            return Util.callAsync(this, this[method], data);
         }

         throw new RpcException(`The method ${this.constructor.name}.${method} is not a function`);
      }
   }
}
