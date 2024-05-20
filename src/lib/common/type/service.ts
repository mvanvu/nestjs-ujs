import { ClassConstructor, RequestRegistryData } from './common';
import { ObjectRecord, Registry, IsEquals } from '@mvanvu/ujs';
import { DMMF } from '@prisma/client/runtime/library';

export type PrismaModels = Record<string, DMMF.Model>;

export interface GetPrismaModels {
   get models(): PrismaModels;
}

export type CRUDServiceOptions<
   TPrismaService extends GetPrismaModels = any,
   TCreateDTO extends ObjectRecord = any,
   TUpdateDTO extends ObjectRecord = Partial<TCreateDTO>,
   TPrismaSelect extends ObjectRecord = any,
   TPrismaInclude extends ObjectRecord = any,
> = {
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
   include?: TPrismaInclude;
   list?: {
      orderFields?: string[];
      searchFields?: string[];
      filterFields?: string[];
      maxLimit?: number;
   };
   softDelete?: boolean;
   events?: {
      onBeforeCreate?: (data: TCreateDTO) => any | Promise<any>;
      onBeforeUpdate?: <TRecord extends ObjectRecord>(data: TUpdateDTO, record: TRecord) => any | Promise<any>;
      onBeforeDelete?: <TRecord extends ObjectRecord>(record: TRecord) => any | Promise<any>;
      onEntity?:
         | ClassConstructor<any>
         | (<TContext extends 'read' | 'create' | 'update' | 'delete'>(
              record: ObjectRecord,
              options: { context: TContext; isList?: IsEquals<TContext, 'read' extends true ? boolean : never> },
           ) => any | Promise<any>);
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

export type UpdateResult<T> = {
   data: T;
   meta: { diff: Record<string, { from: any; to: any }> };
};

export type CRUDResult<T> = T | PaginationResult<T> | UpdateResult<T>;

export type MessageMeta = {
   query?: ObjectRecord;
   params?: ObjectRecord;
   headers?: RequestRegistryData;
   CRUD?: { method: 'read' | 'write' | 'delete'; where?: Record<string, any> };
};

export type MessageData<TData = any, TMeta = MessageMeta | Registry<MessageMeta>> = {
   data?: TData;
   meta?: TMeta;
};

export type BootServiceOptions<TPatterns, TPermissions> = {
   proxy: string;
   patterns: TPatterns;
   permissions?: TPermissions;
};

export class ConfigService<
   TPatterns extends Record<string, string>,
   TPermissions extends Record<string, Record<string, string>> | undefined = undefined,
> {
   constructor(private readonly options: BootServiceOptions<TPatterns, TPermissions>) {}

   get proxy(): string {
      return this.options.proxy;
   }

   get patterns(): TPatterns {
      return this.options.patterns;
   }

   get permissions(): TPermissions | undefined {
      return this.options.permissions;
   }
}
