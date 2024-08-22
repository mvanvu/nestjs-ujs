import { HttpStatus, Injectable } from '@nestjs/common';
import { CRUDService } from '@microservice/@library';
import { CRUDParamsConstructor, PrismaModelName, ThrowException } from '@shared-library';
import { ClassConstructor } from '@mvanvu/ujs';
import { DMMF } from '@prisma/client/runtime/library';

export function CreatePrismaService<TPrismaClient extends ClassConstructor<any>>(
   PrismaClientRef: TPrismaClient,
   models: DMMF.Datamodel['models'],
) {
   class BasePrismaService extends PrismaClientRef {
      async onModuleInit() {
         await this.$connect();
      }

      async onApplicationShutdown() {
         await this.$disconnect();
      }

      createCRUDService<
         TEntity extends ClassConstructor<any>,
         TCreateDTO extends ClassConstructor<any>,
         TUpdateDTO extends ClassConstructor<any>,
      >(
         modelName: PrismaModelName<InstanceType<typeof PrismaClientRef>>,
         options: Omit<CRUDParamsConstructor<TEntity, TCreateDTO, TUpdateDTO>, 'meta' | 'modelName' | 'dataModels'>,
      ): CRUDService<InstanceType<typeof PrismaClientRef>, TEntity, TCreateDTO, TUpdateDTO> {
         if (!this[modelName]) {
            ThrowException(`The model(${modelName}) doesn't exists`, HttpStatus.NOT_IMPLEMENTED);
         }

         const dataModels = {};
         models.forEach((model) => (dataModels[model.name] = model));

         return new CRUDService(this as any, { ...options, modelName, dataModels, meta: this['meta'] });
      }
   }

   return BasePrismaService;
}
