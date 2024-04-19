import { Registry } from '@mvanvu/ujs';
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

export type ServieConstant = {
   proxy: string;
   patterns: Record<string, string>;
};
