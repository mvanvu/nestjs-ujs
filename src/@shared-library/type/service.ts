import { ClassConstructor, HttpRequest, ServiceOptions } from './common';
import { IsEqual, ObjectRecord, Registry } from '@mvanvu/ujs';
import { DMMF } from '@prisma/client/runtime/library';
import { type PaginationQueryDto } from './dto';
import { type UserEntity } from '@shared-library/entity/user';
import { type SystemConfigDto } from '@shared-library/dto/system-config';

export type PrismaModels = Record<string, DMMF.Model>;

export interface GetPrismaModels {
   get models(): PrismaModels;
}

export type PrismaModelName<PrismaService> = keyof Omit<
   PrismaService,
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

export type UpdateResult<T> = { data: T; meta: { diff: Record<string, { from: any; to: any }> } };

export type MetaResult<T> = { data: T; meta: ObjectRecord };

export type CRUDResult<T> = T | PaginationResult<T> | UpdateResult<T>;

export type MessageMeta = {
   query?: ObjectRecord;
   user?: UserEntity;
   method: 'GET' | 'POST' | 'PATCH' | 'DELETE';
   systemConfig?: SystemConfigDto;
};

export type MessageData<TData = any, TMeta = MessageMeta | Registry<MessageMeta>> = {
   data?: TData;
   meta?: TMeta;
};

export type OnServiceResponse = {
   success: boolean;
   messagePattern: string;
   httpRequest: HttpRequest;
   requestData?: { data?: any; meta?: ObjectRecord };
   responseData?: { data?: any; meta?: ObjectRecord };
};

export type CRUDContext = 'read' | 'create' | 'update' | 'delete';

export type CRUDExecuteContext = 'create' | 'update' | 'delete';

export type OnTransactionOptions<
   TRecord extends ObjectRecord,
   TData extends ObjectRecord,
   TContext extends CRUDExecuteContext,
> = {
   context: TContext;
   data: TContext extends 'create' | 'update' ? TData : never;
   record: TRecord;
   oldRecord: TContext extends 'update' ? TRecord : never;
};

export type OnBeforeExecuteOptions<
   TRecord extends ObjectRecord,
   TData extends ObjectRecord,
   TContext extends CRUDExecuteContext,
> = {
   context: TContext;
   data: TContext extends 'create' | 'update' ? TData : never;
   record: TContext extends 'delete' | 'update' ? TRecord : never;
};

export type OnEntityOptions<TContext extends CRUDContext> = {
   context: TContext;
   isList: IsEqual<TContext, 'read' extends true ? boolean : never>;
};

export type OnBeforeExecute<
   TRecord extends ObjectRecord,
   TData extends ObjectRecord,
   TContext extends CRUDExecuteContext,
> = (options: OnBeforeExecuteOptions<TRecord, TData, TContext>) => any | Promise<any>;

export type OnTransaction<
   TX,
   TRecord extends ObjectRecord,
   TData extends ObjectRecord,
   TContext extends CRUDExecuteContext,
> = (tx: TX, options: OnTransactionOptions<TRecord, TData, TContext>) => any | Promise<any>;

export type OnEntity<TEntity extends ObjectRecord, TContext extends CRUDContext> =
   | ClassConstructor<TEntity>
   | ((record: TEntity, options: OnEntityOptions<TContext>) => any | Promise<any>);

export type PaginationListOptions = {
   itemsPerPage: number;
};

export type CRUDClient = {
   read: <TResult>(id: string, optionsOveride?: ServiceOptions) => Promise<TResult>;
   paginate: <TResult>(query?: PaginationQueryDto & ObjectRecord, optionsOveride?: ServiceOptions) => Promise<TResult>;
   create: <TResult, TData>(data: TData, optionsOveride?: ServiceOptions) => Promise<TResult>;
   update: <TResult, TData>(id: string, data: TData, optionsOveride?: ServiceOptions) => Promise<TResult>;
   delete: <TResult>(id: string, optionsOveride?: ServiceOptions) => Promise<TResult>;
};
