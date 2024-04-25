import { appConfig } from '@lib/core/config';
import { ThrowException } from '@lib/exception';
import { CRUDServiceOptions, QueryParams, OrderBy, OrderDirection, PaginationResult } from '@lib/type';
import { DateTime, ObjectRecord, Registry, Transform, Util } from '@mvanvu/ujs';
import { Injectable, Logger } from '@nestjs/common';
import { Operation } from '@prisma/client/runtime/library';

@Injectable()
export class CRUDService<
   PrismaModel extends Record<string | Operation, any>,
   CreateDto extends ObjectRecord,
   PrismaSelect = undefined,
> {
   readonly logger: Logger;

   constructor(public readonly options: CRUDServiceOptions<PrismaModel, PrismaSelect>) {
      this.logger = new Logger(this.constructor.name);
   }

   get model(): PrismaModel {
      return this.options.model;
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
      const model = this.options.model;
      const [items, totalCount] = await Promise.all([
         model.findMany(modelParams),
         model.count({ where: modelParams.where }),
      ]);

      let data = items;

      if (this.options.events?.onEntity) {
         data = await Promise.all(
            items.map((item: T) => Util.callAsync(this, this.options.events?.onEntity || item, item)),
         );
      }

      return { data, meta: { totalCount, page, limit } };
   }

   async getById<T>(id: string): Promise<T> {
      const record = await this.options.model['findUnique']({ where: { id } });

      if (!record) {
         ThrowException(`Record with ID(${id}) not found`);
      }

      return this.options.events?.onEntity ? await Util.callAsync(this, this.options.events.onEntity, record) : record;
   }

   async create<T>(dto: CreateDto): Promise<T> {}
}
