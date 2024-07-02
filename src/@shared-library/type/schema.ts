import { ClassConstructor, IsValidType } from '@mvanvu/ujs';
import { SwaggerOptions } from './common';

export type Each = boolean | 'unique';
export interface BaseSchemaOptions {
   each?: Each;
   optional?: boolean;
   nullable?: boolean;
   swagger?: SwaggerOptions;
}

export interface StringSchemaOptions extends BaseSchemaOptions {
   format?: 'email' | 'url' | 'ipV4' | 'creditCard' | 'date-time' | 'mongoId' | RegExp;
   transform?: 'safeHtml' | 'trim' | false;
   equalsTo?: string;
   minLength?: number;
   maxLength?: number;
   notEmpty?: boolean;
}

export interface NumberSchemaOptions extends BaseSchemaOptions {
   integer?: boolean;
   min?: number;
   max?: number;
   fromString?: boolean;
}

export interface BooleanSchemaOptions extends BaseSchemaOptions {
   fromString?: boolean;
}

export interface PasswordSchemaOptions extends BaseSchemaOptions {
   noSpaces: boolean;
   minLength?: number;
   maxLength?: number;
   minLower?: number;
   minUpper?: number;
   minNumber?: number;
   minSpecialChars?: number;
   equalsTo?: string;
}

export type IsSchemaType = IsValidType | ClassConstructor<any>;
