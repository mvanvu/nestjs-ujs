import { ClassConstructor, ObjectRecord } from '@mvanvu/ujs';
import { HttpStatus, RequestMethod } from '@nestjs/common';
import { Type, VersionValue } from '@nestjs/common/interfaces';
import { type UserEntity } from '../entity/user';
import { Request } from 'express';

export type HttpCacheOptions = {
   disabled?: boolean;
   withUserIdPrefix?: boolean;
   cacheKey?: string;
   cacheRefKeys?: string | RegExp | Array<string | RegExp>;
};

export type UserRole = { id: string; name: string; permissions: string[] }[];

export type DeviceType = 'web' | 'mobile' | 'desktop';
export type DeviceRequest = {
   type: DeviceType;
   name: string;
   shortName: string;
   platform: string;
   version: string;
};

export interface HttpRequest extends Request {
   cacheRefKeys?: HttpCacheOptions['cacheRefKeys'];
   user?: UserEntity;
}

export type ExecuteOptions<TInput> = {
   messagePattern: string;
   clientProxy: string;
   data?: TInput;
   timeout?: number;
   excuter: ((...args: any[]) => any | Promise<any>) | (new (...args: any[]) => any | Promise<any>) | object;
};

export type ServiceConfig = {
   proxy: string;
   patterns: Record<string, string>;
};

export type ServiceOptions = {
   params?: Record<string, string | number | boolean>;
   timeOut?: number;
   noEmitEvent?: boolean;
   meta?: ObjectRecord;
   entityResponse?: ClassConstructor<any>;
};

export type IRouteOptions = {
   pattern: string;
   route?: {
      method?: RequestMethod;
      path?: string | string[];
      version?: VersionValue;
      public?: boolean;
      httpStatus?: HttpStatus;
   };
   swagger?: {
      summary?: string;
      responseType?: any;
      example?: any;
   };
};

export type EntityConstructor<T> = new (...args: any[]) => T;

export type ValidationCode = string | number | ObjectRecord;

export type SwaggerOptions =
   | {
        description?: string;
        type?: Type<unknown> | Function | [Function] | string | Record<string, any>;
        enum?: any[] | Record<string, any>;
        example?: any;
        readOnly?: boolean;
     }
   | false;

export type PermissionOptions = { key?: string; or?: string[]; and?: string[]; adminScope?: boolean } | string;

export type MailerTransporter = 'SMTP';

export type ApiFinalResponse = {
   success?: boolean;
   message?: string;
   data?: any;
   meta?: ObjectRecord;
};
