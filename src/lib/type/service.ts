import { type CRUDService } from '@lib/service';
import { RequestRegistryData, ServiceExecuteOptions } from './common';
import { Registry, Callable } from '@mvanvu/ujs';
import { DMMF } from '@prisma/client/runtime/library';

export type PrismaModels = Record<string, DMMF.Model>;

export interface GetPrismaModels {
   get models(): PrismaModels;
}

export type CRUDServiceOptions<TPrismaService extends GetPrismaModels, TPrismaSelect> = {
   prisma: TPrismaService;
   model: keyof Omit<
      TPrismaService,
      | '$on'
      | '$connect'
      | '$disconnect'
      | '$use'
      | '$transaction'
      | '$runCommandRaw'
      | '$extends'
      | 'enums'
      | 'models'
      | '_models'
      | 'onModuleInit'
      | 'onApplicationShutdown'
      | symbol
   >;
   select?: TPrismaSelect;
   list?: {
      orderFields?: string[];
      searchFields?: string[];
      filterFields?: string[];
      maxLimit?: number;
   };
   events?: {
      onInit?: Callable;
      onEntity?: Callable;
   };
};

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

export type PaginationResult<T> = {
   data: T[];
   meta: {
      totalCount: number;
      page: number;
      limit: number;
   };
};

export type DataDelivery<TData = any> = {
   data: TData;
   meta: RequestRegistryData & { query?: QueryParams; params?: ServiceExecuteOptions['params'] };
};

export type MessageData<TData = any> = {
   data: DataDelivery<TData>['data'];
   meta: Registry<DataDelivery<TData>['meta']>;
};

export interface CreateCRUDService {
   createCRUDService(): CRUDService<any, any, any, any>;
}

export type ServiceExecuteResult<TResult> = Promise<TResult | PaginationResult<TResult>>;
