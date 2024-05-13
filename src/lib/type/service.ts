import { type CRUDService } from '@lib/base';
import { ClassConstructor, RequestRegistryData } from './common';
import { Callable, ObjectRecord, Registry } from '@mvanvu/ujs';
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
      onBeforeCreate?: Callable;
      onBeforeUpdate?: Callable;
      onBeforeDelete?: Callable;
      onEntity?: Callable | ClassConstructor<any>;
   };
};

export type OrderDirection = 'asc' | 'desc';

export type OrderBy = Record<string, OrderDirection> | Record<string, Record<string, OrderDirection>>;

export type PaginationResult<T> = {
   data: T[];
   meta: {
      totalCount: number;
      page: number;
      limit: number;
   };
};

export type MessageMeta = {
   query?: ObjectRecord;
   params?: ObjectRecord;
   headers?: RequestRegistryData;
};

export type MessageData<TData = any, TMeta = MessageMeta | Registry<MessageMeta>> = {
   data?: TData;
   meta?: TMeta;
};

export interface CreateCRUDService {
   createCRUDService(): CRUDService<any, any, any, any>;
}

export type ServiceExecuteResult<TResult> = Promise<TResult | PaginationResult<TResult>>;

export type BootServiceOptions<TPatterns, TPermissions> = {
   proxy: string;
   patterns: TPatterns;
   permissions?: TPermissions;
};
