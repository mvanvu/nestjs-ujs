import { RequestRegistryData } from './common';
import { ObjectRecord, Registry } from '@mvanvu/ujs';
import { DMMF } from '@prisma/client/runtime/library';

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

export type OnServiceResponse = {
   messagePattern: string;
   requestData?: any;
   responseData?: any;
};
