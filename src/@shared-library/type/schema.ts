import { ClassConstructor, IsBaseOptions, IsStringOptions, IsStrongPasswordOptions } from '@mvanvu/ujs';
import { SwaggerOptions } from './common';

export interface BaseSchemaOptions extends IsBaseOptions {
   optional?: boolean;
   nullable?: boolean;
   swagger?: SwaggerOptions;
   code?: string;
   message?: string;
}

export interface StringSchemaOptions extends BaseSchemaOptions, IsStringOptions {
   transform?: 'safeHtml' | 'format' | false;
   skipEmpty?: boolean;
}

export interface NumberSchemaOptions extends BaseSchemaOptions {
   integer?: boolean;
   min?: number;
   max?: number;
}

export interface BooleanSchemaOptions extends BaseSchemaOptions {}

export interface ClassSchemaOptions extends BaseSchemaOptions {
   ref: ClassConstructor<any>;
}

export interface EnumSchemaOptions extends Omit<BaseSchemaOptions, 'nullable'> {
   ref: any[];
}

export interface PasswordSchemaOptions extends BaseSchemaOptions, IsStrongPasswordOptions {
   equalsTo?: string;
}

export interface JsonSchemaOptions extends BaseSchemaOptions {}

export type ValidSchema = 'string' | 'number' | 'boolean' | 'enum' | 'password' | 'json' | 'class' | 'prop';

export type PropertySchemaOptions<T extends ValidSchema> = T extends 'string'
   ? StringSchemaOptions
   : T extends 'number'
     ? NumberSchemaOptions
     : T extends 'boolean'
       ? BooleanSchemaOptions
       : T extends 'class'
         ? ClassSchemaOptions
         : T extends 'enum'
           ? EnumSchemaOptions
           : T extends 'password'
             ? PasswordSchemaOptions
             : T extends 'json'
               ? JsonSchemaOptions
               : BaseSchemaOptions;
