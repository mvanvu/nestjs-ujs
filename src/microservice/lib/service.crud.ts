import { appConfig } from '@config';
import { FieldsException, ThrowException } from '@lib/common/exception';
import {
   CRUDServiceOptions,
   OrderBy,
   OrderDirection,
   PaginationResult,
   UpdateResult,
   GetPrismaModels,
} from '@lib/common';
import { DateTime, Is, ObjectRecord, Registry, Transform, Util } from '@mvanvu/ujs';
import { HttpStatus, Injectable, Logger, validateDTO } from '@nestjs/common';
import { BaseService } from './service.base';

@Injectable()
export class CRUDService<
   TPrismaService extends GetPrismaModels = any,
   TCreateDTO extends ObjectRecord = any,
   TUpdateDTO extends ObjectRecord = Partial<TCreateDTO>,
   TPrismaSelect extends ObjectRecord = any,
   TPrismaInclude extends ObjectRecord = any,
> extends BaseService {
   readonly logger: Logger;

   constructor(
      readonly options: CRUDServiceOptions<TPrismaService, TCreateDTO, TUpdateDTO, TPrismaSelect, TPrismaInclude>,
   ) {
      super();
      this.logger = new Logger(this.constructor.name);
   }

   async paginate<T>(query?: ObjectRecord, where?: Record<string, any>): Promise<PaginationResult<T>> {
      const modelParams = {
         select: this.options.select,
         include: this.options.include,
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
                  if (this.options.list?.orderFields?.length) {
                     for (const orderField of this.options.list?.orderFields) {
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

      if (q && this.options.list?.searchFields?.length) {
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
                     qValue = (qValue as string).split(/\s+/g).filter((qv: string) => !!qv.trim());

                     if (qValue.length) {
                        searchCondition = { OR: qValue.map((qv: string) => ({ contains: qv, mode })) };
                     }

                     break;
               }
            }
         }

         for (const searchField of this.options.list?.searchFields) {
            where.push({ [searchField]: searchCondition });
         }

         modelParams.where.OR.push(...where);
      }

      // Take care filter
      const filterFields = this.options.list?.filterFields || [];

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
               hasFilterByStatus = !!modelParams.where[prop].map((obj) => obj['status'] !== undefined);
            }
         } else {
            delete modelParams.where[prop];
         }
      }

      if (this.options.softDelete === true && !hasFilterByStatus) {
         modelParams.where['status'] = { not: 'Trashed' };
      }

      // Take care pagination
      const defaultLimit = appConfig.get('list.limit');
      const maxLimit = this.options.list?.maxLimit ?? appConfig.get('list.maxLimit');
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

      // Prisma model
      const model = this.options.prisma[this.options.model];
      const [items, totalCount] = await Promise.all([
         model['findMany'](modelParams),
         model['count']({ where: modelParams.where }),
      ]);
      let data = items;

      if (this.options.events?.onEntity) {
         data = await Promise.all(items.map((item: T) => this.callOnEntity(item, { context: 'read', isList: true })));
      }

      return { data, meta: { totalCount, page, limit } };
   }

   private async callOnEntity<T>(
      item: T,
      options: { context: 'read' | 'create' | 'update' | 'delete'; isList?: boolean },
   ): Promise<T> {
      const result = await Util.callAsync<T>(this, this.options.events.onEntity, item, options);

      return Is.class(this.options.events.onEntity) ? result : item;
   }

   async read<T>(id: string, where?: Record<string, any>): Promise<T> {
      where = { ...(where ?? {}), id };

      if (this.options.softDelete === true && where['status'] === undefined) {
         where['status'] = { not: 'Trashed' };
      }

      const record = await this.options.prisma[this.options.model]['findFirst']({
         where,
         select: this.options.select,
         include: this.options.include,
      });

      if (!record) {
         ThrowException(`Record with ID(${id}) not found`, HttpStatus.NOT_FOUND);
      }

      return this.options.events?.onEntity
         ? await this.callOnEntity(record, { context: 'read', isList: false })
         : record;
   }

   async validate(dto: TCreateDTO | TUpdateDTO, id?: string): Promise<void> {
      const modelName = Util.uFirst(this.options.model as string);
      const entityModel = this.options.prisma[modelName];
      const model = this.options.prisma.models[modelName];

      // Remove unknown fields
      for (const fieldName in dto) {
         if (!model.fields.find(({ name }) => name === fieldName)) {
            delete dto[fieldName];
         }
      }

      if (Is.emptyObject(Registry.from<any>(dto).omit(['createdBy', 'updatedBy']).valueOf())) {
         // Nothing to update, throw an exception
         ThrowException(`No data to ${id ? 'update' : 'create'}`);
      }

      // Validate some requirements
      const promises: Promise<any>[] = [];
      const fieldsException = new FieldsException();
      const uniqueFields: Record<string, any> = {};

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
         for (const name in uniqueFields) {
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

   async create<T>(data: TCreateDTO): Promise<T> {
      if (this.options.events?.onBeforeCreate) {
         // Trigger an event before handle
         await Util.callAsync(this, this.options.events.onBeforeCreate, data);
      }

      // Validate data
      await this.validate(data);

      const record = await this.options.prisma[this.options.model]['create']({
         data,
         select: this.options.select,
         include: this.options.include,
      });

      return this.options.events?.onEntity ? await this.callOnEntity(record, { context: 'create' }) : record;
   }

   async update<T>(id: string, data: TUpdateDTO): Promise<UpdateResult<T>> {
      const model = this.options.prisma[this.options.model];
      let oldRecord = await model['findFirst']({
         where: { id },
         select: this.options.select,
         include: this.options.include,
      });

      if (!oldRecord) {
         ThrowException(`The record with ID(${id}) doesn't exists`);
      }

      const { onEntity } = this.options.events ?? {};

      if (onEntity) {
         oldRecord = await this.callOnEntity(oldRecord, { context: 'update' });
      }

      if (this.options.events?.onBeforeUpdate) {
         // Trigger an event before handle
         await Util.callAsync(this, this.options.events.onBeforeUpdate, data, oldRecord);
      }

      // Validate
      await this.validate(data, id);

      let record = await this.options.prisma[this.options.model]['update']({
         data,
         select: this.options.select,
         include: this.options.include,
         where: { id },
      });

      if (onEntity) {
         record = await this.callOnEntity(record, { context: 'update' });
      }

      // Parse diff data
      const diff: UpdateResult<T>['meta']['diff'] = {};

      for (const field in oldRecord) {
         const oldValue = oldRecord[field];
         const newValue = record[field];

         if (!Is.equals(oldValue, newValue)) {
            diff[field] = { from: oldValue, to: newValue };
         }
      }

      return { data: <T>record, meta: { diff } };
   }

   async delete<T>(id: string): Promise<T> {
      let record = await this.options.prisma[this.options.model]['findFirst']({
         where: { id },
         select: this.options.select,
         include: this.options.include,
      });

      if (!record) {
         ThrowException(`Record with ID(${id}) not found`);
      }

      if (this.options.events?.onEntity) {
         record = await this.callOnEntity(record, { context: 'delete' });
      }

      if (this.options.events?.onBeforeDelete) {
         // Trigger an event before handle
         await Util.callAsync(this, this.options.events.onBeforeDelete, record);
      }

      if (this.options.softDelete === true) {
         await this.options.prisma[this.options.model]['update']({
            select: { id: true },
            data: { status: 'Trashed' },
            where: { id },
         });
      } else {
         await this.options.prisma[this.options.model]['delete']({ select: { id: true }, where: { id } });
      }

      return record;
   }

   async execute<TResult>(): Promise<CRUDResult<TResult>> {
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
               ? this.read<TResult>(recordId, meta.get('CRUD.where'))
               : this.paginate<TResult>(meta.get('query'), meta.get('CRUD.where'));

         case 'write':
            // Validate data
            const DTOClassRef = recordId ? updateDTO : createDTO;
            const data = await validateDTO(this.ctx.getData(), DTOClassRef);

            if (userId) {
               data[recordId ? 'updatedBy' : 'createdBy'] = userId;
            }

            return recordId ? this.update<TResult>(recordId, data) : this.create<TResult>(data);

         case 'delete':
            return this.delete<TResult>(recordId);
      }
   }
}
