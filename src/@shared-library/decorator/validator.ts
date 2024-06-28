import { PropertyOptions, SwaggerOptions, TransformType } from '../type/common';
import { Property } from './property';
import { ClassConstructor, Is, IsValidType, Transform, Util } from '@mvanvu/ujs';

type Each = boolean | 'unique';

export function IsString(options?: {
   each?: Each;
   optional?: boolean;
   notEmpty?: boolean;
   trim?: boolean;
   minLength?: number;
   maxLength?: number;
   email?: boolean;
   url?: boolean;
   creditCard?: boolean;
   date?: boolean;
   regex?: RegExp;
   safeHtml?: boolean;
   raw?: boolean;
   swagger?: SwaggerOptions;
   transform?: TransformType | TransformType[];
}) {
   const each = !!options?.each;
   const propsOptions: PropertyOptions<IsValidType> = {
      validate: [],
      optional: options?.optional === true,
      swagger: options?.swagger,
      transform: {
         fromType: 'string',
         toType: Is.array(options?.transform) ? options.transform : options?.transform ? [options.transform] : [],
      },
   };
   const validates = propsOptions.validate as PropertyOptions<IsValidType>['validate'][];
   const transformTo = propsOptions.transform.toType as TransformType[];

   if (options?.safeHtml) {
      transformTo.push('toSafeHtml');
   } else if (options?.raw !== true) {
      transformTo.push('toStripTags');
   }

   // Defaults to trim
   if (options?.trim !== false) {
      transformTo.push('trim');
   }

   if (transformTo.length) {
      propsOptions.transform.toType = Transform.toArrayUnique(transformTo);
   } else {
      delete propsOptions.transform;
   }

   // Validate as email
   if (options?.email) {
      validates.push({ is: 'email', each });
   }
   // Validate as credit card
   else if (options?.creditCard) {
      validates.push({ is: 'creditCard', each });
   }
   // Validate as date string
   else if (options?.date) {
      validates.push({ is: 'dateString', each });
   }
   // Validate as URL string
   else if (options?.url) {
      validates.push({ is: 'url', each });
   } else {
      validates.push({ is: 'string', each });
   }

   // Validate as not empty
   if (options?.notEmpty) {
      validates.push({ is: 'empty', not: true, each });
   }

   if (Is.number(options?.minLength)) {
      validates.push({ is: 'minLength', meta: options.minLength, each });
   }

   if (Is.number(options?.maxLength)) {
      validates.push({ is: 'maxLength', meta: options.maxLength, each });
   }

   if (Is.regex(options?.regex)) {
      validates.push({ is: 'matched', meta: options.regex, each });
   }

   if (options?.each === 'unique') {
      validates.push({ is: 'arrayUnique' });
   }

   return Property(propsOptions);
}

export function StringToType(
   transform: 'int' | 'uInt' | 'number' | 'uNumber' | 'boolean',
   options?: {
      optional?: boolean;
      unsigned?: boolean;
      signed?: boolean;
      integer?: boolean;
      bigInt?: boolean;
      min?: number;
      max?: number;
      swagger?: SwaggerOptions;
   },
) {
   const propsOptions: PropertyOptions<IsValidType> = {
      validate: [{ is: transform }],
      optional: options?.optional === true, // Defaults to required
      swagger: options?.swagger,
      transform: { fromType: 'string', toType: ['trim', `to${Util.uFirst(transform)}`] as TransformType[] },
   };
   const validates = propsOptions.validate as PropertyOptions<IsValidType>['validate'][];

   if (Is.number(options?.min)) {
      validates.push({ is: 'min', meta: options.min });
   }

   if (Is.number(options?.max)) {
      validates.push({ is: 'max', meta: options.max });
   }

   return Property(propsOptions);
}

export function IsNumber(options?: {
   each?: Each;
   optional?: boolean;
   unsigned?: boolean;
   signed?: boolean;
   integer?: boolean;
   bigInt?: boolean;
   min?: number;
   max?: number;
   swagger?: SwaggerOptions;
}) {
   let numType = options?.integer ? 'int' : options?.bigInt ? 'bigInt' : 'number';

   if (options?.unsigned) {
      numType = `u${Util.uFirst(numType)}`;
   } else if (options?.signed) {
      numType = `s${Util.uFirst(numType)}`;
   }

   const each = !!options?.each;
   const propsOptions: PropertyOptions<IsValidType> = {
      validate: [{ is: numType as IsValidType, each }],
      optional: options?.optional === true, // Defaults to required
      swagger: options?.swagger,
   };
   const validates = propsOptions.validate as PropertyOptions<IsValidType>['validate'][];

   if (Is.number(options?.min)) {
      validates.push({ is: 'min', meta: options.min, each });
   }

   if (Is.number(options?.max)) {
      validates.push({ is: 'max', meta: options.max, each });
   }

   if (options?.each === 'unique') {
      validates.push({ is: 'arrayUnique' });
   }

   return Property(propsOptions);
}

export function IsBoolean(options?: { each?: boolean; optional?: boolean; swagger?: SwaggerOptions }) {
   return Property({
      validate: { is: 'boolean', each: !!options?.each },
      optional: options?.optional === true,
      swagger: options?.swagger,
   });
}

export function IsPrimitive(options?: { each?: Each; optional?: boolean; swagger?: SwaggerOptions }) {
   const each = !!options?.each;
   const propsOptions: PropertyOptions<IsValidType> = {
      validate: [{ is: 'primitive' as IsValidType, each }],
      optional: options?.optional === true, // Defaults to required
      swagger: options?.swagger,
   };
   const validates = propsOptions.validate as PropertyOptions<IsValidType>['validate'][];

   if (options?.each === 'unique') {
      validates.push({ is: 'arrayUnique' });
   }

   return Property(propsOptions);
}

export function IsStrongPassword(options?: {
   each?: Each;
   optional?: boolean;
   noSpaces: boolean;
   minLength?: number;
   maxLength?: number;
   minLower?: number;
   minUpper?: number;
   minNumber?: number;
   minSpecialChars?: number;
   swagger?: SwaggerOptions;
}) {
   const minLength = options?.minLength ?? 8;
   const noSpaces = options?.noSpaces ?? true;
   const minSpecialChars = options?.minSpecialChars ?? 1;
   const minUpper = options?.minUpper ?? 1;
   const minLower = options?.minLower ?? 1;
   const minNumber = options?.minNumber ?? 1;
   const each = !!options?.each;
   const propsOptions: PropertyOptions<IsValidType> = {
      validate: [
         {
            is: 'strongPassword' as IsValidType,
            meta: { minLength, minLower, minNumber, minSpecialChars, minUpper, noSpaces },
            each,
         },
      ],
      optional: options?.optional === true, // Defaults to required
      swagger: options?.swagger,
   };
   const validates = propsOptions.validate as PropertyOptions<IsValidType>['validate'][];

   if (options?.each === 'unique') {
      validates.push({ is: 'arrayUnique' });
   }

   return Property(propsOptions);
}

export function IsJsonObject(options?: { each?: Each; optional?: boolean; swagger?: SwaggerOptions }) {
   const each = !!options?.each;
   const propsOptions: PropertyOptions<IsValidType> = {
      validate: [{ is: 'flatObject', each }],
      optional: options?.optional === true, // Defaults to required
      swagger: options?.swagger,
   };
   const validates = propsOptions.validate as PropertyOptions<IsValidType>['validate'][];

   if (options?.each === 'unique') {
      validates.push({ is: 'arrayUnique' });
   }

   return Property(propsOptions);
}

export function IsEquals(equalsTo: string, options?: { each?: Each; optional?: boolean; swagger?: SwaggerOptions }) {
   const each = !!options?.each;
   const propsOptions: PropertyOptions<IsValidType> = {
      validate: [{ is: 'equals', meta: { equalsTo }, each }],
      optional: options?.optional === true, // Defaults to required
      swagger: options?.swagger,
   };
   const validates = propsOptions.validate as PropertyOptions<IsValidType>['validate'][];

   if (options?.each === 'unique') {
      validates.push({ is: 'arrayUnique' });
   }

   return Property(propsOptions);
}

export function IsIn(array: any[], options?: { each?: Each; optional?: boolean; swagger?: SwaggerOptions }) {
   const each = !!options?.each;
   const propsOptions: PropertyOptions<IsValidType> = {
      validate: [{ is: 'inArray', meta: array, each }],
      optional: options?.optional === true, // Defaults to required
      swagger: { enum: array, ...(options?.swagger || {}) },
   };
   const validates = propsOptions.validate as PropertyOptions<IsValidType>['validate'][];

   if (options?.each === 'unique') {
      validates.push({ is: 'arrayUnique' });
   }

   return Property(propsOptions);
}

export function IsDTO(
   DTO: ClassConstructor<any>,
   options?: { each?: Each; optional?: boolean; swagger?: SwaggerOptions },
) {
   const each = !!options?.each;
   const propsOptions: PropertyOptions<IsValidType> = {
      validate: [{ is: DTO, each }],
      optional: options?.optional === true, // Defaults to required
      swagger: { type: each ? [DTO] : DTO, ...(options?.swagger || {}) },
   };
   const validates = propsOptions.validate as PropertyOptions<IsValidType>['validate'][];

   if (options?.each === 'unique') {
      validates.push({ is: 'arrayUnique' });
   }

   return Property(propsOptions);
}

export function IsMongoId(options?: { each?: Each; optional?: boolean; swagger?: SwaggerOptions }) {
   const each = !!options?.each;
   const propsOptions: PropertyOptions<IsValidType> = {
      validate: [{ is: 'mongoId', each }],
      optional: options?.optional === true, // Defaults to required
      swagger: options?.swagger,
   };
   const validates = propsOptions.validate as PropertyOptions<IsValidType>['validate'][];

   if (options?.each === 'unique') {
      validates.push({ is: 'arrayUnique' });
   }

   return Property(propsOptions);
}

export function IsArray(options?: {
   each?: Each;
   optional?: boolean;
   notEmpty?: boolean;
   unique?: boolean;
   swagger?: SwaggerOptions;
}) {
   const each = !!options?.each;
   const propsOptions: PropertyOptions<IsValidType> = {
      validate: [{ is: 'array', each }],
      optional: options?.optional === true,
      swagger: options?.swagger,
   };
   const validates = propsOptions.validate as PropertyOptions<IsValidType>['validate'][];

   if (options?.notEmpty) {
      validates.push({ is: 'empty', not: true, each });
   }

   if (options?.unique) {
      validates.push({ is: 'arrayUnique', each });
   }

   if (options?.each === 'unique') {
      validates.push({ is: 'arrayUnique' });
   }

   return Property(propsOptions);
}
