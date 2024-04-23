import { Registry } from '@mvanvu/ujs';
import { RequestMethod } from '@nestjs/common';
import { VersionValue } from '@nestjs/common/interfaces';
import { Request } from 'express';

export interface HttpRequest extends Request {
   registry?: Registry;
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
      method: RequestMethod;
      path?: string | string[];
      version?: VersionValue;
      public?: boolean;
   };
};
