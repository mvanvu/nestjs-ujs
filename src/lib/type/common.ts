import { CommonType, IsValidOptions, IsValidType, Registry, Transform } from '@mvanvu/ujs';
import { HttpStatus, RequestMethod } from '@nestjs/common';
import { VersionValue } from '@nestjs/common/interfaces';
import { Request } from 'express';

export type RequestRegistryData = {
   user?: { id: string };
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

export type ServieConfig = {
   proxy: string;
   patterns: Record<string, string>;
};

export type ServiceOptions = {
   config: ServieConfig;
};

export type ServiceExecuteOptions = {
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
   swagger?: { description?: string; example?: any };
   validate?: ValidationOptions<IsType> | ValidationOptions<IsType>[];
   transform?: TransformOptions;
   optional?: boolean;
};
