import { RequestRegistryData } from './common';
import { Callable, Registry } from '@mvanvu/ujs';

export type CRUDServiceOptions<TPrismaModel, TPrismaSelect> = {
   model: TPrismaModel;
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
   meta: RequestRegistryData & { query: QueryParams };
};

export type MessageData<TData = any> = {
   data: DataDelivery<TData>['data'];
   meta: Registry<DataDelivery<TData>['meta']>;
};
