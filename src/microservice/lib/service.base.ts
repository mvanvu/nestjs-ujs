import { ThrowException } from '@lib/exception';
import { MessageMeta } from '@lib';
import { Is, Registry, Util } from '@mvanvu/ujs';
import { Inject, Injectable } from '@nestjs/common';
import { CONTEXT, RequestContext } from '@nestjs/microservices';
import { CRUDService } from './service.crud';

export interface CreateCRUDService {
   createCRUDService(): CRUDService<any, any, any, any>;
}

@Injectable()
export class BaseService {
   @Inject(CONTEXT) readonly ctx: RequestContext;

   async execute<TInput, TResult>(data?: TInput): Promise<TResult> {
      const {
         properties: { headers },
      } = this.ctx.getContext().getMessage();
      const messagePattern = this.ctx.getPattern();
      const meta = Registry.from<MessageMeta>(headers?.['x-meta']);
      const method: string = messagePattern.split('.').pop();
      const isCRUDPattern = ['paginate', 'read', 'create', 'update', 'delete'].includes(method);

      if (isCRUDPattern && messagePattern.includes('.CRUD.') && typeof this['createCRUDService'] === 'function') {
         const CRUDInstService = Util.call(this, this['createCRUDService']);

         if (CRUDInstService instanceof CRUDService) {
            const recordId = meta.get('params.id');
            const userId = meta.get('headers.user.id');

            switch (method) {
               case 'paginate':
                  return CRUDInstService.paginate<TResult>(meta.get('query')) as unknown as TResult;

               case 'read':
                  return CRUDInstService.read<TResult>(recordId);

               case 'create':
                  if (userId) {
                     data['createdBy'] = userId;
                  }

                  return CRUDInstService.create<TResult>(data);

               case 'update':
                  if (userId) {
                     data['updatedBy'] = userId;
                  }

                  return CRUDInstService.update<TResult>(recordId, data);

               case 'delete':
                  return CRUDInstService.delete<TResult>(recordId);
            }
         }
      }

      if (Is.callable(this[method])) {
         return Util.callAsync(this, this[method], data);
      }

      ThrowException(`The method ${this.constructor.name}.${method} is not a function`);
   }
}
