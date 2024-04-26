import { appConfig } from '@lib/core/config';
import { FieldsException, ThrowException } from '@lib/exception';
import { CRUDServiceOptions, QueryParams, OrderBy, OrderDirection, PaginationResult, GetPrismaModels } from '@lib/type';
import { DateTime, Is, ObjectRecord, Registry, Transform, Util } from '@mvanvu/ujs';
import { Injectable, Logger } from '@nestjs/common';

@Injectable()
export class CRUDService<
   PrismaService extends GetPrismaModels,
   PrismaSelect extends ObjectRecord,
   CreateDto extends ObjectRecord,
   UpdateDto extends ObjectRecord = Partial<CreateDto>,
> {
   readonly logger: Logger;

   constructor(public readonly options: CRUDServiceOptions<PrismaService, PrismaSelect>) {
      this.logger = new Logger(this.constructor.name);
   }

   async paginate<T>(query?: QueryParams): Promise<PaginationResult<T>> {
      const modelParams = {
         select: this.options.select,
         where: { AND: [], OR: [] },
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
                     qValue = qValue.split(/\s+/g).filter((qv) => !!qv.trim());

                     if (qValue.length) {
                        searchCondition = { OR: qValue.map((qv) => ({ contains: qv, mode })) };
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

      for (const prop of ['OR', 'AND']) {
         if (!modelParams.where[prop].length) {
            delete modelParams.where[prop];
         }
      }

      // Take care pagination
      const defaultLimit = appConfig.list.limit;
      const maxLimit = this.options.list?.maxLimit ?? appConfig.list.maxLimit;
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
         data = await Promise.all(
            items.map((item: T) =>
               Util.callAsync(this, this.options.events?.onEntity || item, item, { context: 'read', isList: true }),
            ),
         );
      }

      return { data, meta: { totalCount, page, limit } };
   }

   async read<T>(id: string): Promise<T> {
      const record = await this.options.model['findUnique']({ where: { id } });

      if (!record) {
         ThrowException(`Record with ID(${id}) not found`);
      }

      return this.options.events?.onEntity
         ? await Util.callAsync(this, this.options.events.onEntity, record, { context: 'read', isList: false })
         : record;
   }

   async validate(dto: CreateDto | UpdateDto, id?: string): Promise<void> {
      const entityModel = this.options.prisma[this.options.model];
      const model = this.options.prisma.models[`${this.options.model}`];

      // Validate unknown fields
      for (const fieldName in dto) {
         const field = model.fields.find(({ name }) => name === fieldName);

         if (!field) {
            ThrowException(`Unknown the field name: ${fieldName}`);
         }
      }

      // Validate some requirements
      const promises: Promise<any>[] = [];
      const fieldsException = new FieldsException();
      const uniqueFields: Record<string, any>[] = [];

      for (const field of model.fields) {
         const { name } = field;
         const value = dto[name];
         const isNothing = Is.nullOrUndefined(value);

         if (field.isRequired && isNothing && !field.hasDefaultValue) {
            fieldsException.add(name, FieldsException.REQUIRED);
         }

         if (field.isUnique && !isNothing) {
            uniqueFields.push({ [name]: value });
         }
      }

      if (
         !Is.emptyObject(uniqueFields) &&
         Is.equals(Util.sort(Object.keys(uniqueFields)), Util.sort(model.uniqueFields))
      ) {
         for (const name in uniqueFields) {
            promises.push(
               entityModel['findFirst']({
                  where: { [name]: uniqueFields[name], id: id ? { id: { not: id } } : undefined },
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

   async create<T>(data: CreateDto): Promise<T> {
      // Validate first
      await this.validate(data);

      if (Is.emptyObject(data)) {
         // Nothing to create, throw an exception
         ThrowException('No data to create');
      }

      if (this.options.events?.onInit) {
         // Trigger an event before handle
         await Util.callAsync(this, this.options.events.onInit, data, { context: 'create' });
      }

      const record = await this.options.prisma[this.options.model]['create']({
         data,
         select: this.options.select,
      });

      if (this.options.events?.onEntity) {
         // Trigger an event before response
         await Util.callAsync(this, this.options.events.onEntity, record, { data, context: 'create' });
      }

      return record;
   }

   async update<T>(id: string, data: UpdateDto): Promise<T> {
      // Validate first
      await this.validate(data, id);
      const model = this.options.prisma[this.options.model];
      const oldRecord = await model['findFirst']({ where: { id }, select: this.options.select });

      if (!oldRecord) {
         ThrowException(`The record with ID(${id}) doesn't exists`);
      }

      if (Is.emptyObject(data)) {
         // Nothing to update, throw an exception
         ThrowException('No data to update');
      }

      if (this.options.events?.onInit) {
         // Trigger an event before handle
         await Util.callAsync(this, this.options.events.onInit, data, { oldRecord, context: 'update' });
      }

      const record = await this.options.prisma[this.options.model]['update']({
         data,
         select: this.options.select,
         where: { id },
      });

      if (this.options.events?.onEntity) {
         // Trigger an event before response
         await Util.callAsync(this, this.options.events.onEntity, record, {
            oldRecord,
            data,
            context: 'update',
         });
      }

      return record;
   }

   async delete<T>(id: string): Promise<T> {
      if (this.options.events?.onInit) {
         // Trigger an event before handle
         await Util.callAsync(this, this.options.events.onInit, id, { context: 'delete' });
      }

      const record = await this.options.prisma[this.options.model]['delete']({
         select: this.options.select,
         where: { id },
      });

      if (this.options.events?.onEntity) {
         // Trigger an event before response

         await Util.callAsync(this, this.options.events.onEntity, record, { context: 'delete' });
      }

      return record;
   }
}
