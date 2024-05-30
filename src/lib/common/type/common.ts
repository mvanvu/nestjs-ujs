import { CommonType, IsValidOptions, IsValidType, Registry, Transform } from '@mvanvu/ujs';
import { HttpStatus, RequestMethod } from '@nestjs/common';
import { Type, VersionValue } from '@nestjs/common/interfaces';
import { type UserEntity } from '@lib/service/user/entity/user';
import { Request } from 'express';

export type UserRole = { id: string; name: string; permissions: string[] }[];

export type RequestRegistryData = {
   user?: UserEntity;
   tz?: string;
   device: 'web' | 'mobile' | 'desktop';
   userAgent: string;
   ipAddress: string;
};

export interface HttpRequest extends Request {
   registry: Registry<RequestRegistryData>;
}

export type ClassConstructor<T> = new (...arg: any[]) => T;

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

export type ValidationCode = string | number;

export type ValidationOptions<T> = {
   is: T;
   each?: boolean;
   not?: boolean;
   meta?: IsValidOptions<T>['meta'];
   code?: ValidationCode;
};

export type TransformType<T = keyof typeof Transform> = T extends 'clean' | 'cleanIfType' | 'prototype' ? never : T;

export type TransformOptions = {
   toType: TransformType | TransformType[];
   fromType?: CommonType | CommonType[];
};

export type PropertyOptions<IsType extends IsValidType> = {
   validate?: ValidationOptions<IsType> | ValidationOptions<IsType>[];
   transform?: TransformOptions;
   optional?: boolean;
   swagger?: {
      disable?: boolean;
      description?: string;
      type?: Type<unknown> | Function | [Function] | string | Record<string, any>;
      enum?: any[] | Record<string, any>;
   };
};

export type PermissionOptions = { key?: string; or?: string[]; and?: string[] } | string;
