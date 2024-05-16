import { ThrowException } from '@lib/exception';
import { CRUDResult, ClassConstructor, MessageMeta, validateDTO } from '@lib';
import { Is, Registry, Util } from '@mvanvu/ujs';
import { HttpStatus, Inject, Injectable } from '@nestjs/common';
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

      if (Is.callable(this[method])) {
         return Util.callAsync(this, this[method], data);
      }

      ThrowException(`The method ${this.constructor.name}.${method} is not a function`);
   }

   async executeCRUD<TResult, TCreateDTO, TUpdateDTO>(
      createDTO: ClassConstructor<TCreateDTO>,
      updateDTO: ClassConstructor<TUpdateDTO>,
   ): Promise<CRUDResult<TResult>> {
      if (!Is.callable(this['createCRUDService'])) {
         ThrowException(
            `Must implement createCRUDService for the service: ${this.constructor.name}`,
            HttpStatus.NOT_IMPLEMENTED,
         );
      }

      const CRUDInstService = Util.call(this, this['createCRUDService']);

      if (CRUDInstService instanceof CRUDService) {
         const meta = this.meta;
         const recordId = meta.get('params.id');
         const userId = meta.get('headers.user.id');
         const method = meta.get('CRUD.method');

         if (!['read', 'write', 'delete'].includes(method)) {
            ThrowException(
               `The header sending message method must be one of (read, write, delete)`,
               HttpStatus.NOT_IMPLEMENTED,
            );
         }

         switch (method) {
            case 'read':
               return recordId
                  ? CRUDInstService.read<TResult>(recordId, meta.get('CRUD.where'))
                  : CRUDInstService.paginate<TResult>(meta.get('query'), meta.get('CRUD.where'));

            case 'write':
               // Validate data
               const DTOClassRef = recordId ? updateDTO : createDTO;
               const data = await validateDTO(this.ctx.getData(), DTOClassRef);

               if (userId) {
                  data[recordId ? 'updatedBy' : 'createdBy'] = userId;
               }

               return recordId
                  ? CRUDInstService.update<TResult>(recordId, data)
                  : CRUDInstService.create<TResult>(data);

            case 'delete':
               return CRUDInstService.delete<TResult>(recordId);
         }
      } else {
         ThrowException(
            `The createCRUDService must return an instance of CRUDService class`,
            HttpStatus.NOT_IMPLEMENTED,
         );
      }
   }
}
