import { type BaseEntity } from '@lib/entity';
import { RequestRegistryData } from './common';
import { BaseDMMF } from '@prisma/client/runtime/library';
import { Registry } from '@mvanvu/ujs';
export type PrismaModels = BaseDMMF['datamodel']['models'];
export type PrismaModel = PrismaModels[0];
export type PrismaModelField = PrismaModel['fields'][0];

export type CRUDServiceOptions<PrismaService, PrismaSelect> = {
   prisma: PrismaService;
   modelName: string;
   select?: PrismaSelect;
   entity?: typeof BaseEntity;
};

export interface BasePrismaService {
   readonly dmmf: BaseDMMF;
}

export type CRUDServiceModelFields = Record<string, Record<string, PrismaModelField>>;

export type OrderDirection = 'asc' | 'desc';

export type OrderBy = Record<string, OrderDirection> | Record<string, Record<string, OrderDirection>>;

export type QueryParams = {
   scope?: string;
   q?: string;
   page?: number;
   limit?: number;
   lang?: string;
   order?: string;
};

export type PaginationParams = {
   query?: QueryParams;
   orderFields?: string[];
   searchFields?: string[];
   filterFields?: string[];
   maxLimit?: number;
};

export type PaginationResult<T> = {
   data: T[];
   meta: {
      totalCount: number;
      page: number;
      limit: number;
   };
};

export type DataDelivery<TData = any> = { data: TData; meta: Registry<RequestRegistryData> };
