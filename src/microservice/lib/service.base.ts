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

   get meta(): Registry<MessageMeta> {
      const {
         properties: { headers },
      } = this.ctx.getContext().getMessage();

      return Registry.from<MessageMeta>(headers?.['x-meta']);
   }

   async execute<TInput, TResult>(data?: TInput): Promise<TResult> {
      const messagePattern = this.ctx.getPattern();
      const method: string = messagePattern.split('.').pop();
      const isCRUDPattern =
         ['read', 'write', 'delete'].includes(method) &&
         messagePattern.includes('.CRUD.') &&
         typeof this['createCRUDService'] === 'function';

      if (isCRUDPattern) {
         const CRUDInstService = Util.call(this, this['createCRUDService']);

         if (CRUDInstService instanceof CRUDService) {
            const meta = this.meta;
            const recordId = meta.get('params.id');
            const userId = meta.get('headers.user.id');

            switch (method) {
               case 'read':
                  return recordId
                     ? CRUDInstService.read<TResult>(recordId)
                     : (CRUDInstService.paginate<TResult>(meta.get('query')) as unknown as TResult);

               case 'write':
                  if (userId) {
                     data[recordId ? 'updatedBy' : 'createdBy'] = userId;
                  }

                  return recordId
                     ? CRUDInstService.update<TResult>(recordId, data)
                     : CRUDInstService.create<TResult>(data);

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
