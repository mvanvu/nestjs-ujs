import { FieldsException, ThrowException } from '@shared-library/exception';
import {
   OrderBy,
   OrderDirection,
   PaginationResult,
   UpdateResult,
   CRUDResult,
   validateDTO,
   availableStatuses,
   CRUDContext,
   EntityResult,
   BaseEntity,
   CRUDParamsConstructor,
   parseQueryParamSearch,
   parseQueryParamFilter,
} from '@shared-library';
import {
   Callable,
   ClassConstructor,
   ClassRefSchema,
   DateTime,
   Is,
   ObjectRecord,
   Registry,
   Schema,
   Transform,
   TransformType,
   Util,
} from '@mvanvu/ujs';
import { HttpStatus, Injectable, Logger } from '@nestjs/common';

@Injectable()
export class CRUDService<
   TPrismaService extends object,
   TEntity extends ClassConstructor<any>,
   TCreateDTO extends ClassConstructor<any>,
   TUpdateDTO extends ClassConstructor<any>,
> {
   readonly logger: Logger;

   private prismaSelect?: ObjectRecord;

   private prismaInclude?: ObjectRecord;

   private events: {
      onBoot?: Callable;
      onBeforePaginate?: Callable;
      onBeforeCreate?: Callable;
      onAfterCreate?: Callable;
      onBeforeUpdate?: Callable;
      onAfterUpdate?: Callable;
      onBeforeDelete?: Callable;
      onAfterDelete?: Callable;
   } = {};

   private configCRUD?: {
      softDelete?: boolean;
      list?: {
         orderFields?: string[];
         searchFields?: string[];
         filterFields?: string[];
         defaultLimit?: number;
         maxLimit?: number;
      };
   };

   constructor(
      private readonly prisma: TPrismaService,
      private readonly params: CRUDParamsConstructor<TEntity, TCreateDTO, TUpdateDTO>,
   ) {
      this.logger = new Logger(this.constructor.name);
   }

   select<T extends ObjectRecord>(select?: T): this {
      this.prismaSelect = select;

      return this;
   }

   include<T extends ObjectRecord>(include?: T): this {
      this.prismaInclude = include;

      return this;
   }

   config(configCRUD?: typeof this.configCRUD): this {
      this.configCRUD = configCRUD;

      return this;
   }

   boot<TData>(cb: (options?: { data?: TData; context: CRUDContext }) => any | Promise<any>): this {
      this.events.onBoot = cb as Callable;

      return this;
   }

   beforePaginate(
      cb: (options: {
         q: string;
         query: Record<string, any>;
         modelParams: {
            select?: any;
            include?: any;
            where?: ObjectRecord;
            orderBy?: OrderBy[];
            take?: number;
            skip?: number;
         };
      }) => any | Promise<any>,
   ): this {
      this.events.onBeforePaginate = cb as Callable;

      return this;
   }

   beforeCreate(cb: (options: { data: InstanceType<TCreateDTO>; tx: TPrismaService }) => any | Promise<any>): this {
      this.events.onBeforeCreate = cb as Callable;

      return this;
   }

   afterCreate(
      cb: (options: {
         record: InstanceType<TEntity>;
         data: InstanceType<TCreateDTO>;
         tx: TPrismaService;
      }) => any | Promise<any>,
   ): this {
      this.events.onAfterCreate = cb as Callable;

      return this;
   }

   beforeUpdate(
      cb: (options: {
         record: InstanceType<TEntity>;
         data: InstanceType<TUpdateDTO>;
         tx: TPrismaService;
      }) => any | Promise<any>,
   ): this {
      this.events.onBeforeUpdate = cb as Callable;

      return this;
   }

   afterUpdate(
      cb: (options: {
         previous: InstanceType<TEntity>;
         record: InstanceType<TEntity>;
         data: InstanceType<TUpdateDTO>;
         tx: TPrismaService;
      }) => any | Promise<any>,
   ): this {
      this.events.onAfterUpdate = cb as Callable;

      return this;
   }

   beforeDelete(cb: (options: { record: InstanceType<TEntity>; tx: TPrismaService }) => any | Promise<any>): this {
      this.events.onBeforeDelete = cb as Callable;

      return this;
   }

   afterDelete(cb: (options: { record: InstanceType<TEntity>; tx: TPrismaService }) => any | Promise<any>): this {
      this.events.onAfterDelete = cb as Callable;

      return this;
   }

   async paginate<T>(query?: ObjectRecord): Promise<PaginationResult<T>> {
      const modelParams = {
         select: this.prismaSelect,
         include: this.prismaInclude,
         where: { OR: [], AND: [] } as ObjectRecord,
         orderBy: <OrderBy[]>[],
         take: undefined,
         skip: undefined,
      };

      query = query || {};

      // Take care order by
      const orderBy = <OrderBy | string>(query?.order || '');

      if (orderBy) {
         if (typeof orderBy === 'string') {
            const orderByArray = orderBy.split(',');

            for (const order of orderByArray) {
               // eslint-disable-next-line prefer-const
               let [ordering, direction] = order.split(' ');
               direction = direction?.toLowerCase() ?? undefined;

               if (direction === undefined) {
                  direction = 'asc';
               }

               if (['asc', 'desc'].includes(direction)) {
                  if (this.configCRUD?.list?.orderFields?.length) {
                     for (const orderField of this.configCRUD.list.orderFields) {
                        const regex = /\[([a-z0-9_.,]+)\]/gi;
                        let fieldName = orderField;
                        let queryName = fieldName;

                        if (fieldName.match(regex)) {
                           queryName = fieldName.replace(regex, '');
                           fieldName = fieldName.replace(queryName, '').replace(regex, '$1');
                        }

                        if (ordering === queryName) {
                           if (fieldName.includes(',')) {
                              const multiSortFields = fieldName.split('.');
                              const lastSortField = multiSortFields.pop();
                              const prefix = multiSortFields.join('.');

                              if (prefix) {
                                 modelParams.orderBy.push(
                                    ...lastSortField.split(',').map((name) => ({
                                       [prefix]: { [name]: <OrderDirection>direction },
                                    })),
                                 );
                              } else {
                                 modelParams.orderBy.push(
                                    ...fieldName.split(',').map((name) => ({ [name]: <OrderDirection>direction })),
                                 );
                              }
                           } else {
                              modelParams.orderBy.push(Registry.from().set(fieldName, direction).valueOf());
                           }
                        }
                     }
                  }
               }
            }
         } else {
            modelParams.orderBy.push(orderBy);
         }
      }

      // Check to set default ordering
      if (!modelParams.orderBy.length) {
         if (
            this.params.dataModels[Util.uFirst(this.params.modelName)].fields.find(({ name }) => name === 'createdAt')
         ) {
            modelParams.orderBy.push({ createdAt: 'desc' });
         }
      }

      // Take care search
      const q = (query.q || '').toString().trim();
      const searchCondition = parseQueryParamSearch(q);

      if (searchCondition && this.configCRUD?.list?.searchFields?.length) {
         const where: Record<string, any>[] = [];

         for (const searchField of this.configCRUD.list.searchFields) {
            where.push({ [searchField]: searchCondition });
         }

         modelParams.where.OR.push(...where);
      }

      // Take care filter
      const filterFields = this.configCRUD?.list?.filterFields || [];

      if (filterFields.length) {
         filterFields.forEach((field) => {
            const filterCondition = parseQueryParamFilter(field, query);

            if (filterCondition) {
               modelParams.where.AND.push(filterCondition);
            }
         });
      }

      // Take care pagination
      const defaultLimit = this.configCRUD?.list?.defaultLimit ?? 25;
      const maxLimit = this.configCRUD?.list?.maxLimit ?? 1000;
      let limit: number =
         query.limit === undefined || !query.limit.toString().match(/^[0-9]+$/)
            ? defaultLimit
            : Transform.toNumber(query.limit);
      if (limit === 0 || (maxLimit && maxLimit < limit)) {
         limit = maxLimit;
      }

      const page = Transform.toNumber(query.page) || 1;
      modelParams.take = limit;
      modelParams.skip = (page - 1) * limit;
      Object.assign(query, { page, limit, q });

      if (Is.callable(this.events.onBeforePaginate)) {
         await Util.call(this, this.events.onBeforePaginate, { query, q, modelParams });
      }

      // Take care soft delete status
      let hasFilterByStatus: boolean = modelParams.where['status'];

      // Clean up OR & AND where
      for (const prop of ['OR', 'AND']) {
         if (modelParams.where[prop].length) {
            if (!hasFilterByStatus) {
               hasFilterByStatus = !!modelParams.where[prop].map((obj: ObjectRecord) => obj['status'] !== undefined);
            }
         } else {
            delete modelParams.where[prop];
         }
      }

      if (this.configCRUD?.softDelete === true && !hasFilterByStatus) {
         modelParams.where['status'] = { not: availableStatuses.Trashed };
      }

      // Prisma model
      const model = this.prisma[this.params.modelName];
      const [items, totalCount] = await Promise.all([
         model['findMany'](modelParams),
         model['count']({ where: modelParams.where }),
      ]);
      let data = items;

      if (this.params.entity) {
         data = await Promise.all(items.map((item: T) => this.callOnEntity(item, 'read', true)));
      }

      const message = this.params.meta
         .get('language')
         ._(`RESULT_FOUND_${totalCount > 1 ? 'N' : totalCount > 0 ? '1' : '0'}`, {
            count: totalCount,
         });

      return { message, data, meta: { totalCount, page, limit } };
   }

   private async callOnEntity<T, TContext extends CRUDContext>(
      item: T,
      context: TContext,
      isList?: boolean,
   ): Promise<T> {
      if (Is.class(this.params.entity)) {
         return BaseEntity.bindToClass(item, this.params.entity);
      }

      return await Util.callAsync<T>(this, this.params.entity, item, { context, isList });
   }

   async read<T>(id: string, where?: Record<string, any>): Promise<EntityResult<T>> {
      where = { ...(where ?? {}), id };

      if (this.configCRUD?.softDelete === true && where['status'] === undefined) {
         where['status'] = { not: availableStatuses.Trashed };
      }

      const record = await this.prisma[this.params.modelName]['findFirst']({
         where,
         select: this.prismaSelect,
         include: this.prismaInclude,
      });
      const language = this.params.meta.get('language');

      if (!record) {
         ThrowException(language._('ITEM_ID_NOT_FOUND', { id }), HttpStatus.NOT_FOUND);
      }

      return {
         data: this.params.entity ? await this.callOnEntity(record, 'read') : record,
         message: language._('RESULT_FOUND_1'),
      };
   }

   async validate<TData extends ObjectRecord>(dto: TData, id?: string): Promise<void> {
      const language = this.params.meta.get('language');
      const modelName = Util.uFirst(this.params.modelName);
      const entityModel = this.prisma[this.params.modelName];
      const model = this.params.dataModels[modelName];

      // Remove unknown fields
      for (const fieldName in dto) {
         if (!model.fields.find(({ name }) => name === fieldName)) {
            delete dto[fieldName];
         }
      }

      if (Registry.from<any>(dto).omit(['createdBy', 'updatedBy', 'createdAt', 'updatedAt']).isEmpty()) {
         // Nothing to update, throw an exception
         ThrowException(language._('EMPTY_DATA_WARN'));
      }

      // Validate some requirements
      const promises: Promise<any>[] = [];
      const fieldsException = new FieldsException();
      const uniqueFields: ObjectRecord = {};

      for (const field of model.fields) {
         const { name } = field;
         const value = dto[name];
         const isNothing = value === undefined || value === null;

         if (!id && field.isRequired && isNothing && !field.relationName && !field.hasDefaultValue) {
            fieldsException.add(name, FieldsException.REQUIRED, language._('FIELD_REQUIRED', { field: name }));
         }

         if (field.isUnique && !isNothing) {
            uniqueFields[name] = value;
         }
      }

      if (!Is.empty(uniqueFields)) {
         for (const name in <ObjectRecord>uniqueFields) {
            promises.push(
               entityModel['findFirst']({
                  where: {
                     [name]: { equals: uniqueFields[name], mode: 'insensitive' },
                     id: id ? { not: id } : undefined,
                  },
                  select: { id: true },
               }).then((record: { id: string }) => {
                  if (!!record) {
                     fieldsException.add(name, FieldsException.UNIQUE_CONSTRAINT, language._('UNIQUE_CONSTRAINT_WARN'));
                  }
               }),
            );
         }
      }

      if (promises.length) {
         await Promise.all(promises);
      }

      fieldsException.validate();
   }

   async create<TResult, TData extends ObjectRecord>(data: TData): Promise<EntityResult<TResult>> {
      const record = await this.prisma['$transaction'](async (tx: TPrismaService) => {
         if (Is.callable(this.events.onBeforeCreate)) {
            // Trigger an event before return results
            await Util.callAsync(this, this.events.onBeforeCreate, { tx, data });
         }

         // Validate data
         await this.validate(data);

         const record = await tx[this.params.modelName]['create']({
            data,
            select: this.prismaSelect,
            include: this.prismaInclude,
         });

         if (Is.callable(this.events.onAfterCreate)) {
            // Trigger an event before return results
            await Util.callAsync(this, this.events.onAfterCreate, { tx, record, data });
         }

         return record;
      });

      return {
         data: this.params.entity ? await this.callOnEntity(record, 'create') : record,
         message: this.params.meta.get('language')._('ITEM_CREATED'),
      };
   }

   async update<TResult, TData extends ObjectRecord>(id: string, data: TData): Promise<UpdateResult<TResult>> {
      const language = this.params.meta.get('language');
      let previous = await this.prisma[this.params.modelName]['findFirst']({
         where: { id },
         select: this.prismaSelect,
         include: this.prismaInclude,
      });

      if (!previous) {
         ThrowException(language._('ITEM_ID_NOT_FOUND', { id }), HttpStatus.NOT_FOUND);
      }

      if (this.params.entity) {
         previous = await this.callOnEntity(previous, 'update');
      }

      const record = await this.prisma['$transaction'](async (tx: TPrismaService) => {
         if (Is.callable(this.events.onBeforeUpdate)) {
            // Trigger an event before return results
            await Util.callAsync(this, this.events.onBeforeUpdate, { tx, data, record: previous });
         }

         // Validate data
         await this.validate(data, id);

         let record = await tx[this.params.modelName]['update']({
            data,
            select: this.prismaSelect,
            include: this.prismaInclude,
            where: { id },
         });

         if (this.params.entity) {
            record = await this.callOnEntity(record, 'update');
         }

         if (Is.callable(this.events.onAfterUpdate)) {
            // Trigger an event before return results
            await Util.callAsync(this, this.events.onAfterUpdate, { tx, data, previous, record });
         }

         return record;
      });

      // Parse diff data
      const diff: UpdateResult<TResult>['meta']['diff'] = {};

      for (const field in previous) {
         const oldValue = previous[field];
         const newValue = record[field];

         if (!Is.equals(oldValue, newValue)) {
            diff[field] = { from: oldValue, to: newValue };
         }
      }

      return { data: <TResult>record, meta: { diff }, message: language._('ITEM_UPDATED') };
   }

   async delete<TResult>(id: string): Promise<EntityResult<TResult>> {
      const language = this.params.meta.get('language');
      let record = await this.prisma[this.params.modelName]['findFirst']({
         where: { id },
         select: this.prismaSelect,
         include: this.prismaInclude,
      });

      if (!record) {
         ThrowException(language._('ITEM_ID_NOT_FOUND', { id }), HttpStatus.NOT_FOUND);
      }

      if (this.params.entity) {
         record = await this.callOnEntity(record, 'delete');
      }

      await this.prisma['$transaction'](async (tx: TPrismaService) => {
         if (this.events.onBeforeDelete) {
            // Trigger an event before handle
            await Util.callAsync(this, this.events.onBeforeDelete, { tx, record });
         }

         if (this.configCRUD?.softDelete === true) {
            await tx[this.params.modelName]['update']({
               select: { id: true },
               data: { status: availableStatuses.Trashed },
               where: { id },
            });

            record.status = availableStatuses.Trashed;
         } else {
            await tx[this.params.modelName]['delete']({ select: { id: true }, where: { id } });
         }

         if (Is.callable(this.events.onAfterDelete)) {
            // Trigger an event before return results
            await Util.callAsync(this, this.events.onAfterDelete, { tx, record });
         }
      });

      return { data: record, message: language._('ITEM_DELETED') };
   }

   async execute<TResult>(): Promise<CRUDResult<TResult>> {
      const meta = this.params.meta;
      const method = meta.get('method');
      const dto = meta.get('ctx').getData();
      const isMongoId = Schema.mongoId().check(dto);
      const context =
         method === 'GET'
            ? 'read'
            : method === 'POST'
              ? 'create'
              : method === 'PATCH'
                ? 'update'
                : method === 'DELETE'
                  ? 'delete'
                  : 'unknown';

      if (context === 'unknown') {
         ThrowException('CRUD request wrong method or data', HttpStatus.NOT_IMPLEMENTED);
      }

      if (Is.callable(this.events.onBoot)) {
         // Trigger an event before handle
         await Util.callAsync(this, this.events.onBoot, { context, data: dto });
      }

      if (method === 'GET') {
         return isMongoId ? this.read<TResult>(dto) : this.paginate<TResult>(dto);
      }

      if (method === 'DELETE' && isMongoId) {
         return this.delete<TResult>(dto);
      }

      const userRef = meta.get('user', null);

      if (method === 'PATCH' && Is.object(dto) && Schema.mongoId().check(dto.id) && Is.object(dto.data)) {
         const dtoClassRef =
            this.params.updateDto ??
            (this.params.updateDto ? ClassRefSchema.Partial(this.params.createDto) : undefined);
         const data = dtoClassRef ? validateDTO(dto.data, dtoClassRef) : dto.data;

         if (userRef) {
            // Append some dynamic user data
            data.editor = userRef;
         }

         return this.update<TResult, any>(dto.id, data);
      }

      if (method === 'POST' && Is.object(dto)) {
         const data = this.params.createDto ? validateDTO(dto, this.params.createDto) : dto;

         if (userRef) {
            // Append some dynamic user data
            data.author = userRef;
         }

         return this.create<TResult, any>(data);
      }

      ThrowException('CRUD request wrong method or data', HttpStatus.NOT_IMPLEMENTED);
   }
}
