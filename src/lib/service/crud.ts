import { appConfig } from '@lib/core/config';
import { BaseEntity } from '@lib/entity';
import {
   CRUDServiceOptions,
   PrismaModel,
   PrismaModelField,
   BasePrismaService,
   CRUDServiceModelFields,
   QueryParams,
   OrderBy,
   OrderDirection,
   PaginationResult,
} from '@lib/type';
import { DateTime, ObjectRecord, Registry, Transform } from '@mvanvu/ujs';
import { Injectable, Logger } from '@nestjs/common';
import { RpcException } from '@nestjs/microservices';

@Injectable()
export class CRUDService<PrismaService extends BasePrismaService, PrismaSelect = any> {
   protected readonly logger: Logger = new Logger(CRUDService.name);

   readonly select: PrismaSelect;

   protected modelFields: CRUDServiceModelFields = {};

   constructor(public readonly options: CRUDServiceOptions<PrismaService, PrismaSelect>) {
      this.logger = new Logger(this.constructor.name);
      options.prisma.dmmf.datamodel.models.forEach((model: PrismaModel) => {
         const name = model.name.toLowerCase();
         this.modelFields[name] = {};
         model.fields.forEach((field: PrismaModelField) => (this.modelFields[name][field.name] = field));
      });
   }

   private parseDeepCondition(
      path: string,
      deepModelFields: Record<string, PrismaModelField>,
      condition: Record<string, any> | any[],
      isSearch = true,
      deepListKey = 'some',
   ) {
      const deepPath = path.split('.');
      const conditionsPath = [];
      const lastPath = deepPath.pop();

      while (deepPath.length) {
         const part = deepPath.shift();
         conditionsPath.push(part);

         if (deepModelFields[part].isList) {
            conditionsPath.push(deepListKey);
         }

         deepModelFields = this.getModelFields(deepModelFields[part].type);
      }

      if (isSearch && process.env.MULTILINGUAL && deepModelFields?.translations?.type?.match(/Translation$/g)) {
         return Registry.from()
            .set(conditionsPath.join('.'), {
               OR: [
                  {
                     translations: {
                        some: {
                           referenceField: lastPath,
                           translatedValue: condition,
                        },
                     },
                  },
                  { [lastPath]: condition },
               ],
            })
            .valueOf();
      }

      conditionsPath.push(lastPath);

      return Registry.from().set(conditionsPath.join('.'), condition).valueOf();
   }

   private getModelFields(modelName: string): Record<string, PrismaModelField> {
      modelName = modelName.toLowerCase();

      if (!this.modelFields[modelName]) {
         throw new RpcException('Prisma model name is undefined');
      }

      return this.modelFields[modelName];
   }

   protected willTranslate(lang?: string) {
      const enableTranslation: boolean = Transform.toBoolean(process.env.MULTILINGUAL);
      const defaultLanguage: string = process.env.DEFAULT_LANGUAGE || 'en-GB';
      const allowLanguages: string[] = (process.env.ALLOW_LANGUAGE || '*').split(',');

      return enableTranslation && [...allowLanguages, '*'].includes(lang) && (lang === '*' || lang !== defaultLanguage);
   }

   async paginate<TResult>(query?: QueryParams): Promise<PaginationResult<TResult>> {
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
      const modelFields = this.getModelFields(this.options.modelName);

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
                  if (modelFields[ordering] && !modelFields[ordering].relationName) {
                     modelParams.orderBy.push({
                        [ordering]: <OrderDirection>direction,
                     });
                  } else if (this.options.list?.orderFields?.length) {
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
            if (searchField.includes('.')) {
               where.push(this.parseDeepCondition(searchField, modelFields, searchCondition));
            } else {
               where.push({ [searchField]: searchCondition });
            }
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

                  if (fieldName.includes('.')) {
                     Object.assign(
                        modelParams.where,
                        this.parseDeepCondition(fieldName, modelFields, { gte: from.native, lt: to.native }, false),
                     );
                  } else {
                     modelParams.where[fieldName] = { gte: from.native, lt: to.native };
                  }
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

               if (fieldName.includes('.')) {
                  if (inValues.length) {
                     orWhere.push(this.parseDeepCondition(fieldName, modelFields, { in: inValues }, false));
                  }

                  if (notInValues.length) {
                     orWhere.push(this.parseDeepCondition(fieldName, modelFields, { notIn: notInValues }, false));
                     orWhere.push(this.parseDeepCondition(fieldName, modelFields, {}, false, 'none'));
                  }
               } else {
                  if (inValues.length) {
                     orWhere.push({ [fieldName]: { in: inValues } });
                  }

                  if (notInValues.length) {
                     orWhere.push({ [fieldName]: { notIn: notInValues } });
                  }
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
      const model = (this.options.prisma as any)[this.options.modelName];
      const [items, totalCount] = await Promise.all([
         model['findMany'](modelParams),
         model['count']({ where: modelParams.where }),
      ]);

      const data = this.options.entity
         ? items.map((dt: ObjectRecord) => BaseEntity.bindToClass(this.options.entity, dt))
         : items;

      return { data, meta: { totalCount, page, limit } };
   }
}
