import { ClassConstructor, HttpRequest, RequestRegistryData } from './common';
import { IsEqual, ObjectRecord, Registry } from '@mvanvu/ujs';
import { DMMF } from '@prisma/client/runtime/library';
import { type PaginationQueryDto } from './dto';

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
   params?: ObjectRecord;
   headers?: RequestRegistryData;
   CRUD?: { method: 'read' | 'write' | 'delete'; where?: Record<string, any> };
};

export type MessageData<TData = any, TMeta = MessageMeta | Registry<MessageMeta>> = {
   data?: TData;
   meta?: TMeta;
};

export type OnServiceResponse = {
   messagePattern: string;
   httpRequest: HttpRequest;
   requestData?: any;
   responseData?: any;
   success: boolean;
};

export type CRUDContext = 'read' | 'create' | 'update' | 'delete';

export type CRUDWriteContext = 'create' | 'update';

export type CRUDTransactionContext = 'create' | 'update' | 'delete';

export type OnBeforeSave<TData extends ObjectRecord, TRecord extends ObjectRecord> = (
   data: TData,
   options?: { record?: TRecord; context: CRUDWriteContext },
) => any | Promise<any>;
export type OnBeforeCreate<TData extends ObjectRecord = any> = (data: TData) => any | Promise<any>;
export type OnBeforeUpdate<TData extends ObjectRecord = any, TRecord extends ObjectRecord = any> = (
   data: TData,
   record: TRecord,
) => any | Promise<any>;
export type OnBeforeDelete<TRecord extends ObjectRecord> = (record: TRecord) => any | Promise<any>;
export type OnEntity =
   | ClassConstructor<any>
   | (<TRecord extends ObjectRecord = any, TContext extends CRUDContext = any>(
        record: TRecord,
        options: { context: TContext; isList?: IsEqual<TContext, 'read' extends true ? boolean : never> },
     ) => any | Promise<any>);
export type OnTransaction<TX, TData extends ObjectRecord> = (tx: TX, data: TData) => Promise<any>;

export type PaginationListOptions = {
   itemsPerPage: number;
};

export type CRUDClient = {
   read: <TResult>(id: string) => Promise<TResult>;
   paginate: <TResult>(query?: PaginationQueryDto) => Promise<TResult>;
   create: <TResult, TData>(data: TData) => Promise<TResult>;
   update: <TResult, TData>(id: string, data: TData) => Promise<TResult>;
   delete: <TResult>(id: string) => Promise<TResult>;
};
