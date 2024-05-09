import { ThrowException } from '@lib/exception';
import { metadata } from '../metadata';
import {
   HttpRequest,
   MessageData,
   MessageMeta,
   ServiceExecuteOptions,
   ServiceExecuteResult,
   ServiceOptions,
} from '../type';
import { Is, Registry, Util } from '@mvanvu/ujs';
import { Inject, Injectable } from '@nestjs/common';
import { REQUEST } from '@nestjs/core';
import { ClientProxy, RmqRecordBuilder, CONTEXT, RequestContext } from '@nestjs/microservices';
import { lastValueFrom, timeout } from 'rxjs';
import { CRUDService } from '@lib/service/crud';

@Injectable()
export abstract class BaseService {
   abstract readonly options: ServiceOptions;

   @Inject(REQUEST) readonly req: HttpRequest;

   @Inject(CONTEXT) readonly ctx: RequestContext;

   async execute<TInput, TResult>(
      messagePattern: string,
      dataDelivery: MessageData<TInput>,
      options?: ServiceExecuteOptions,
   ): ServiceExecuteResult<TResult> {
      if (metadata.isGateway()) {
         // Handle for the api gateway
         try {
            const app = metadata.getGateway();
            const client = app.get<ClientProxy>(this.options.config.proxy);
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
               client.send<TResult, typeof record>(messagePattern, record).pipe(timeout(options?.timeOut ?? 5000)),
            );

            // Todo, handle response data
            return response;
         } catch (e) {
            Util.debug(e);
            new ThrowException(e);
         }
      } else {
         const {
            properties: { headers },
         } = this.ctx.getContext().getMessage();
         const meta = Registry.from<MessageMeta>(headers?.['x-meta']);
         const method: string = messagePattern.split('.').pop();
         const isCRUDPattern = ['paginate', 'read', 'create', 'update', 'delete'].includes(method);
         dataDelivery = { data: dataDelivery.data as TInput, meta };

         if (isCRUDPattern && messagePattern.includes('.CRUD.') && typeof this['createCRUDService'] === 'function') {
            const CRUDInstService = Util.call(this, this['createCRUDService']);

            if (CRUDInstService instanceof CRUDService) {
               const recordId = meta.get('params.id');
               const userId = meta.get('headers.user.id');

               switch (method) {
                  case 'paginate':
                     return CRUDInstService.paginate<TResult>(meta.get('query'));

                  case 'read':
                     return CRUDInstService.read<TResult>(recordId);

                  case 'create':
                     if (userId) {
                        dataDelivery.data['createdBy'] = userId;
                     }

                     return CRUDInstService.create<TResult>(dataDelivery.data);

                  case 'update':
                     if (userId) {
                        dataDelivery.data['updatedBy'] = userId;
                     }

                     return CRUDInstService.update<TResult>(recordId, dataDelivery.data);

                  case 'delete':
                     return CRUDInstService.delete<TResult>(recordId);
               }
            }
         }

         if (Is.callable(this[method])) {
            return Util.callAsync(this, this[method], dataDelivery);
         }

         ThrowException(`The method ${this.constructor.name}.${method} is not a function`);
      }
   }
}
