import { appConfig } from '@metadata';
import { FieldsException, ThrowException } from '@lib/common/exception';
import {
   OrderBy,
   OrderDirection,
   PaginationResult,
   UpdateResult,
   CRUDResult,
   ClassConstructor,
   IPartialType,
   validateDTO,
   availableStatuses,
   OnBeforeSave,
   OnBeforeDelete,
   OnEntity,
   OnTransaction,
   CRUDTransactionContext,
   CRUDContext,
   PaginationListOptions,
} from '@lib/common';
import { DateTime, Is, ObjectRecord, Registry, Transform, Util } from '@mvanvu/ujs';
import { HttpStatus, Injectable, Logger } from '@nestjs/common';
import { BaseService } from './service.base';
import { RequestContext } from '@nestjs/microservices';

@Injectable()
export class CRUDService<TPrismaService extends { models: ObjectRecord }> {
   readonly logger: Logger;

   private ctx: RequestContext;

   private prismaSelect?: ObjectRecord;

   private prismaInclude?: ObjectRecord;

   private createDTO?: ClassConstructor<any>;

   private updateDTO?: ClassConstructor<any>;

   private events: {
      onBeforeSave?: OnBeforeSave<any, any>;
      onBeforeDelete?: OnBeforeDelete<any>;
      onTransaction?: OnTransaction<any, any>;
      onEntity?: OnEntity;
   } = {};

   private optionsCRUD?: {
      softDelete?: boolean;
      list?: {
         orderFields?: string[];
         searchFields?: string[];
         filterFields?: string[];
         maxLimit?: number;
      };
   };

   constructor(
      private readonly prisma: TPrismaService,
      private readonly model: string,
   ) {
      this.logger = new Logger(this.constructor.name);
   }

   setCtx(ctx: RequestContext): this {
      this.ctx = ctx;

      return this;
   }

   select<T extends ObjectRecord>(select?: T): this {
      this.prismaSelect = select;

      return this;
   }

   include<T extends ObjectRecord>(include?: T): this {
      this.prismaInclude = include;

      return this;
   }

   options(options?: typeof this.optionsCRUD): this {
      this.optionsCRUD = options;

      return this;
   }

   beforeSave<TData, TRecord>(callback: OnBeforeSave<TData, TRecord>): this {
      this.events.onBeforeSave = callback;

      return this;
   }

   beforeDelete<TRecord>(callback: OnBeforeDelete<TRecord>): this {
      this.events.onBeforeDelete = callback;

      return this;
   }

   entityResponse(callback: OnEntity): this {
      this.events.onEntity = callback;

      return this;
   }

   transaction<TData, TRecord>(
      fn: (
         tx: TPrismaService,
         options?: { record: TRecord; data: TData; context: CRUDTransactionContext },
      ) => Promise<any>,
   ): this {
      this.events.onTransaction = fn;

      return this;
   }

   validateDTOPipe<TCreateDTO extends ClassConstructor<any>, TUpdateDTO extends ClassConstructor<any> | undefined>(
      createDTO: TCreateDTO,
      updateDTO?: TUpdateDTO,
   ): this {
      this.createDTO = createDTO;
      this.updateDTO = updateDTO;

      return this;
   }

   async paginate<T>(
      query?: ObjectRecord,
      where?: Record<string, any>,
      options?: PaginationListOptions,
   ): Promise<PaginationResult<T>> {
      const modelParams = {
         select: this.prismaSelect,
         include: this.prismaInclude,
         where: where ?? {},
         orderBy: <OrderBy[]>[],
         take: undefined,
         skip: undefined,
      };

      // Init WHERE attributes
      if (!modelParams.where.OR) {
         modelParams.where.OR = [];
      }

      if (!modelParams.where.AND) {
         modelParams.where.AND = [];
      }

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
                  if (this.optionsCRUD?.list?.orderFields?.length) {
                     for (const orderField of this.optionsCRUD.list.orderFields) {
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

      // Take care search
      const q = (query.q || '').toString().trim();

      if (q && this.optionsCRUD?.list?.searchFields?.length) {
         const where: Record<string, any>[] = [];
         const mode = 'insensitive';
         let searchCondition: Record<string, any> = { contains: q, mode };

         // Check if advance search
         for (const markup of ['!', '^', '$', '~', '|']) {
            const index = q.indexOf(markup);
            let qValue: string | string[] = q.substring(markup.length);

            if (index === 0 && qValue) {
               switch (markup) {
                  case '!':
                     searchCondition = { not: { contains: qValue, mode } };
                     break;

                  case '^':
                     searchCondition = { startsWith: qValue, mode };
                     break;

                  case '$':
                     searchCondition = { endsWith: qValue, mode };
                     break;

                  case '~':
                     searchCondition = { equals: qValue, mode };
                     break;

                  case '|':
                     qValue = (qValue as string).split(/[\s\n]+/g).filter((qv: string) => !!qv.trim());

                     if (qValue.length) {
                        searchCondition = { OR: qValue.map((qv: string) => ({ contains: qv, mode })) };
                     }

                     break;
               }
            }
         }

         for (const searchField of this.optionsCRUD.list.searchFields) {
            where.push({ [searchField]: searchCondition });
         }

         modelParams.where.OR.push(...where);
      }

      // Take care filter
      const filterFields = this.optionsCRUD?.list?.filterFields || [];

      if (filterFields.length) {
         for (const field of filterFields) {
            const parts = field.split(':');
            const regex = /\[([a-z0-9_.]+)\]/gi;
            let fieldName = <any>parts[0];
            let queryName = fieldName;

            if (fieldName.match(regex)) {
               queryName = fieldName.replace(regex, '');
               fieldName = fieldName.replace(queryName, '').replace(regex, '$1');
            }

            if (parts[1]?.toUpperCase() === 'DATE') {
               const [fromDate, toDate] = queryName.includes('-') ? queryName.split('-') : [queryName, undefined];
               const from = DateTime.from(query[fromDate]);

               if (from.valid) {
                  let to = DateTime.from(query[toDate] || '');

                  if (!to.valid) {
                     to = from.clone();
                  }

                  from.startOf();
                  to.endOf();
                  modelParams.where[fieldName] = { gte: from.native, lt: to.native };
               }

               continue;
            }

            if (query[queryName] === undefined) {
               continue;
            }

            const queryValue = Transform.toString(query[queryName]);
            const valueEquals = [];
            let valueArray: any[] = queryValue.split('|').map((val) => {
               val = val.trim();

               if (val[0] === '!') {
                  valueEquals.push(false);

                  return val.substring(1) || '';
               }

               valueEquals.push(true);

               return val;
            });
            const castAs = parts[1]?.toLowerCase().split(',') ?? undefined;

            if (castAs) {
               const typeTransform = castAs.filter((asType) =>
                  ['number', 'unumber', 'int', 'uint', 'boolean'].includes(asType),
               );
               valueArray = valueArray.map((value) => Transform.clean(value, typeTransform));
            }

            if (valueArray.length) {
               const orWhere = [];
               const inValues = [];
               const notInValues = [];
               valueArray.forEach((value, index) => {
                  const isEquals = valueEquals[index] === true;

                  if (isEquals) {
                     inValues.push(value);
                  } else {
                     notInValues.push(value);
                  }
               });

               if (inValues.length) {
                  orWhere.push({ [fieldName]: { in: inValues } });
               }

               if (notInValues.length) {
                  orWhere.push({ [fieldName]: { notIn: notInValues } });
               }

               modelParams.where.AND.push(orWhere.length > 1 ? { OR: orWhere } : orWhere[0]);
            }
         }
      }

      // Take care soft delete status
      let hasFilterByStatus: boolean = modelParams.where['status'];

      for (const prop of ['OR', 'AND']) {
         if (modelParams.where[prop].length) {
            if (!hasFilterByStatus) {
               hasFilterByStatus = !!modelParams.where[prop].map((obj: ObjectRecord) => obj['status'] !== undefined);
            }
         } else {
            delete modelParams.where[prop];
         }
      }

      if (this.optionsCRUD?.softDelete === true && !hasFilterByStatus) {
         modelParams.where['status'] = { not: availableStatuses.Trashed };
      }

      // Take care pagination
      const defaultLimit = options?.itemsPerPage ?? 25;
      const maxLimit = this.optionsCRUD?.list?.maxLimit ?? 1000;
      let limit: number =
         query.limit === undefined || !query.limit.toString().match(/^[0-9]+$/)
            ? defaultLimit
            : Transform.toUInt(query.limit);
      if (limit === 0 || (maxLimit && maxLimit < limit)) {
         limit = maxLimit;
      }

      const page = Transform.toUInt(query.page) || 1;
      modelParams.take = limit;
      modelParams.skip = (page - 1) * limit;
      Object.assign(query, { page, limit, q });

      // Query by fields scope
      const scope = appConfig.get('queryScope');

      if (scope && Is.string(query[scope]) && !Is.empty(query[scope])) {
         const { fields } = this.prisma.models[this.model];
         const selectedFields: string[] = query[scope]
            .split(',')
            .filter((field: string) => !!field && !!fields.find(({ name }) => name === field));

         if (selectedFields.length) {
            if (!Is.object(modelParams.select) || Is.emptyObject(modelParams.select)) {
               modelParams.select = selectedFields.map((field) => ({ [field]: true }));
            } else {
               for (const field in <ObjectRecord>modelParams.select) {
                  if (!selectedFields.includes(field)) {
                     delete modelParams.select[field];
                  }
               }

               for (const field of selectedFields) {
                  Object.assign(modelParams.select, { [field]: true });
               }
            }
         }
      }

      // Prisma model
      const model = this.prisma[this.model];
      const [items, totalCount] = await Promise.all([
         model['findMany'](modelParams),
         model['count']({ where: modelParams.where }),
      ]);
      let data = items;

      if (this.events.onEntity) {
         data = await Promise.all(items.map((item: T) => this.callOnEntity(item, { context: 'read', isList: true })));
      }

      return { data, meta: { totalCount, page, limit } };
   }

   private async callOnEntity<T>(item: T, options: { context: CRUDContext; isList?: boolean }): Promise<T> {
      const result = await Util.callAsync<T>(this, this.events.onEntity, item, options);

      return Is.class(this.events.onEntity) ? result : item;
   }

   async read<T>(id: string, where?: Record<string, any>): Promise<T> {
      where = { ...(where ?? {}), id };

      if (this.optionsCRUD?.softDelete === true && where['status'] === undefined) {
         where['status'] = { not: availableStatuses.Trashed };
      }

      const record = await this.prisma[this.model]['findFirst']({
         where,
         select: this.prismaSelect,
         include: this.prismaInclude,
      });

      if (!record) {
         ThrowException(`Record with ID(${id}) not found`, HttpStatus.NOT_FOUND);
      }

      return this.events.onEntity ? await this.callOnEntity(record, { context: 'read', isList: false }) : record;
   }

   async validate<TData extends ObjectRecord>(dto: TData, id?: string): Promise<void> {
      const modelName = Util.uFirst(this.model as string);
      const entityModel = this.prisma[this.model];
      const model = this.prisma.models[modelName];

      // Remove unknown fields
      for (const fieldName in dto) {
         if (!model.fields.find(({ name }) => name === fieldName)) {
            delete dto[fieldName];
         }
      }

      if (Registry.from(dto).omit(['createdBy', 'updatedBy', 'createdAt', 'updatedAt']).isEmpty()) {
         // Nothing to update, throw an exception
         ThrowException(`No data to ${id ? 'update' : 'create'}`);
      }

      // Validate some requirements
      const promises: Promise<any>[] = [];
      const fieldsException = new FieldsException();
      const uniqueFields: ObjectRecord = {};

      for (const field of model.fields) {
         const { name } = field;
         const value = dto[name];
         const isNothing = Is.nothing(value);

         if (!id && field.isRequired && isNothing && !field.relationName && !field.hasDefaultValue) {
            fieldsException.add(name, FieldsException.REQUIRED);
         }

         if (field.isUnique && !isNothing) {
            uniqueFields[name] = value;
         }
      }

      if (!Is.emptyObject(uniqueFields)) {
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
                     fieldsException.add(name, FieldsException.UNIQUE_CONSTRAINT);
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

   async create<TResult, TData extends ObjectRecord>(data: TData): Promise<TResult> {
      if (Is.callable(this.events.onBeforeSave)) {
         // Trigger an event before handle
         await Util.callAsync(this, this.events.onBeforeSave, data, { context: 'create' });
      }

      // Validate data
      await this.validate(data);

      const record = await this.prisma['$transaction'](async (tx: TPrismaService) => {
         const item = await tx[this.model]['create']({
            data,
            select: this.prismaSelect,
            include: this.prismaInclude,
         });

         if (Is.callable(this.events.onTransaction)) {
            // Trigger an event before return results
            await Util.callAsync(this, this.events.onTransaction, tx, { record: item, data, context: 'create' });
         }

         return item;
      });

      return this.events.onEntity ? await this.callOnEntity(record, { context: 'create' }) : record;
   }

   async update<TResult, TData extends ObjectRecord>(id: string, data: TData): Promise<UpdateResult<TResult>> {
      const model = this.prisma[this.model];
      let oldRecord = await model['findFirst']({
         where: { id },
         select: this.prismaSelect,
         include: this.prismaInclude,
      });

      if (!oldRecord) {
         ThrowException(`The record with ID(${id}) doesn't exists`);
      }

      const { onEntity } = this.events;

      if (onEntity) {
         oldRecord = await this.callOnEntity(oldRecord, { context: 'update' });
      }

      if (this.events.onBeforeSave) {
         // Trigger an event before handle
         await Util.callAsync(this, this.events.onBeforeSave, data, { record: oldRecord, context: 'update' });
      }

      // Validate
      await this.validate(data, id);

      let record = await this.prisma['$transaction'](async (tx: TPrismaService) => {
         const item = await tx[this.model]['update']({
            data,
            select: this.prismaSelect,
            include: this.prismaInclude,
            where: { id },
         });

         if (Is.callable(this.events.onTransaction)) {
            // Trigger an event before return results
            await Util.callAsync(this, this.events.onTransaction, tx, { record: item, data, context: 'update' });
         }

         return item;
      });

      if (onEntity) {
         record = await this.callOnEntity(record, { context: 'update' });
      }

      // Parse diff data
      const diff: UpdateResult<TResult>['meta']['diff'] = {};

      for (const field in oldRecord) {
         const oldValue = oldRecord[field];
         const newValue = record[field];

         if (!Is.equals(oldValue, newValue)) {
            diff[field] = { from: oldValue, to: newValue };
         }
      }

      return { data: <TResult>record, meta: { diff } };
   }

   async delete<TResult>(id: string): Promise<TResult> {
      let record = await this.prisma[this.model]['findFirst']({
         where: { id },
         select: this.prismaSelect,
         include: this.prismaInclude,
      });

      if (!record) {
         ThrowException(`Record with ID(${id}) not found`);
      }

      if (this.events.onEntity) {
         record = await this.callOnEntity(record, { context: 'delete' });
      }

      if (this.events.onBeforeDelete) {
         // Trigger an event before handle
         await Util.callAsync(this, this.events.onBeforeDelete, record);
      }

      await this.prisma['$transaction'](async (tx: TPrismaService) => {
         if (this.optionsCRUD?.softDelete === true) {
            await tx[this.model]['update']({
               select: { id: true },
               data: { status: 'Trashed' },
               where: { id },
            });

            record.status = 'Trashed';
         } else {
            await tx[this.model]['delete']({ select: { id: true }, where: { id } });
         }

         if (Is.callable(this.events.onTransaction)) {
            // Trigger an event before return results
            await Util.callAsync(this, this.events.onTransaction, tx, { record, context: 'delete' });
         }
      });

      return record;
   }

   async execute<TResult>(ctx?: RequestContext): Promise<CRUDResult<TResult>> {
      ctx = ctx ?? this.ctx;
      const meta = BaseService.parseMeta(ctx);
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
               ? this.read<TResult>(recordId, meta.get('CRUD.where'))
               : this.paginate<TResult>(meta.get('query'), meta.get('CRUD.where'), {
                    itemsPerPage: meta.get('headers.systemConfig.itemsPerPage'),
                 });

         case 'write':
            // Check to validate data
            const DTOClassRef: ClassConstructor<any> = recordId
               ? this.updateDTO ?? (this.createDTO ? IPartialType(this.createDTO) : undefined)
               : this.createDTO;
            const data = DTOClassRef ? await validateDTO(ctx.getData(), DTOClassRef) : ctx.getData();

            if (userId) {
               data[recordId ? 'updatedBy' : 'createdBy'] = userId;
            }

            return recordId ? this.update<TResult, any>(recordId, data) : this.create<TResult, any>(data);

         case 'delete':
            return this.delete<TResult>(recordId);
      }
   }
}
