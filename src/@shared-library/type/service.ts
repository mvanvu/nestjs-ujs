import { HttpRequest, ServiceOptions } from './common';
import { ClassConstructor, ObjectRecord, Registry } from '@mvanvu/ujs';
import { DMMF } from '@prisma/client/runtime/library';
import { type PaginationQueryDto } from '../dto/pagination-query';
import { type UserRefEntity } from '../entity/user';
import { type Language } from '../i18n';
import { RequestContext } from '@nestjs/microservices';

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
   | 'onModuleInit'
   | 'onApplicationShutdown'
   | symbol
   | number
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

export type CRUDParamsConstructor<
   TEntity extends ClassConstructor<any>,
   TCreateDTO extends ClassConstructor<any>,
   TUpdateDTO extends ClassConstructor<any>,
> = {
   modelName: string;
   dataModels: Record<string, DMMF.Model>;
   meta: MessageMetaProvider;
   entity?: TEntity;
   createDto?: TCreateDTO;
   updateDto?: TUpdateDTO;
};

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
