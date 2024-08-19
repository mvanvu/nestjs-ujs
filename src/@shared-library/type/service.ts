import { ClassConstructor, HttpRequest, ServiceOptions } from './common';
import { IsEqual, ObjectRecord, Registry } from '@mvanvu/ujs';
import { DMMF } from '@prisma/client/runtime/library';
import { type PaginationQueryDto } from '../dto/pagination-query';
import { type UserRefEntity } from '../entity/user';
import { type Language } from '../i18n';
import { RequestContext } from '@nestjs/microservices';
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

export type EntityResult<T> = { message?: string; data: T };

export type PaginationResult<T> = {
   message?: string;
   data: T[];
   meta: {
      totalCount: number;
      page: number;
      limit: number;
   };
};

export type UpdateResult<T> = { data: T; meta: { diff: Record<string, { from: any; to: any }> }; message?: string };

export type DataMetaResult<TData, TMeta = ObjectRecord> = { data: TData; meta: TMeta };

export type CRUDResult<T> = PaginationResult<T> | EntityResult<T> | UpdateResult<T>;

export type CRUDMethod = 'GET' | 'POST' | 'PATCH' | 'DELETE';

export type MessageMeta = {
   query?: ObjectRecord;
   user?: UserRefEntity;
   language: Language;
   method: CRUDMethod;
   ctx: RequestContext;
};

export class MessageMetaProvider extends Registry<MessageMeta> {}

export type MessageData<TData = any, TMeta = MessageMeta | Registry<MessageMeta>> = {
   data?: TData;
   meta?: TMeta;
};

export type OnServiceResponse<TDataResponse = any> = {
   success: boolean;
   messagePattern: string;
   httpRequest: HttpRequest;
   requestData?: { data?: any; meta?: ObjectRecord };
   responseData?: TDataResponse;
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

export type OnAfterTransaction<
   TRecord extends ObjectRecord,
   TData extends ObjectRecord,
   TContext extends CRUDExecuteContext,
> = (options: OnTransactionOptions<TRecord, TData, TContext>) => any | Promise<any>;

export type OnEntity<TEntity extends ObjectRecord, TContext extends CRUDContext> =
   | ClassConstructor<TEntity>
   | ((record: TEntity, options: OnEntityOptions<TContext>) => any | Promise<any>);

export type CRUDClient = {
   read: <TEntity>(id: string, optionsOveride?: ServiceOptions) => Promise<EntityResult<TEntity>>;
   paginate: <TEntity>(
      query?: PaginationQueryDto & ObjectRecord,
      optionsOveride?: ServiceOptions,
   ) => Promise<PaginationResult<TEntity>>;
   create: <TEntity, TData>(data: TData, optionsOveride?: ServiceOptions) => Promise<EntityResult<TEntity>>;
   update: <TEntity, TData>(id: string, data: TData, optionsOveride?: ServiceOptions) => Promise<EntityResult<TEntity>>;
   delete: <TEntity>(id: string, optionsOveride?: ServiceOptions) => Promise<EntityResult<TEntity>>;
};
