import { ThrowException } from '@lib/exception';
import { metadata } from '../metadata';
import {
   DataDelivery,
   HttpRequest,
   MessageData,
   ServiceExecuteOptions,
   ServiceExecuteResult,
   ServiceOptions,
} from '../type';
import { Is, Registry, Util } from '@mvanvu/ujs';
import { Inject, Injectable } from '@nestjs/common';
import { REQUEST } from '@nestjs/core';
import { ClientProxy } from '@nestjs/microservices';
import { lastValueFrom, timeout } from 'rxjs';
import { CRUDService } from '@lib/service/crud';

@Injectable()
export abstract class BaseService {
   abstract readonly options: ServiceOptions;

   @Inject(REQUEST) readonly req: HttpRequest;

   async execute<TInput, TResult>(
      messagePattern: string,
      data: MessageData<TInput> | TInput,
      options?: ServiceExecuteOptions,
   ): ServiceExecuteResult<TResult> {
      if (metadata.isGateway()) {
         // Handle for the api gateway
         const dataDelivery: DataDelivery = {
            data: data ?? null,
            meta: { ...this.req.registry.valueOf(), query: this.req.query, params: options?.params },
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
         const dataDelivery = data as MessageData<TInput>;
         const meta = Registry.from<DataDelivery['meta']>(data['meta']);
         const method: string = messagePattern.split('.').pop();
         const isCRUDPattern = ['paginate', 'read', 'create', 'update', 'delete'].includes(method);

         if (isCRUDPattern && typeof this['createCRUDService'] === 'function') {
            const CRUDInstService = Util.call(this, this['createCRUDService']);

            if (CRUDInstService instanceof CRUDService) {
               const recordId = meta.get('params.id');

               switch (method) {
                  case 'paginate':
                     return CRUDInstService.paginate<TResult>(meta.get('query'));

                  case 'read':
                     return CRUDInstService.read<TResult>(recordId);

                  case 'create':
                     return CRUDInstService.create<TResult>(dataDelivery.data);

                  case 'update':
                     return CRUDInstService.update<TResult>(recordId, dataDelivery.data);

                  case 'delete':
                     return CRUDInstService.delete<TResult>(recordId);
               }
            }
         }

         if (Is.callable(this[method])) {
            return Util.callAsync(this, this[method], { data: dataDelivery.data, meta });
         }

         ThrowException(`The method ${this.constructor.name}.${method} is not a function`);
      }
   }
}
