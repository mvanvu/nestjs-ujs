import { applyDecorators } from '@nestjs/common';
import { ClassConstructor, Is, ObjectRecord } from '@mvanvu/ujs';
import { PropertyOptions } from '../type/common';
import { CLASS_PROPERTIES } from '@shared-library/constant';
import { isGateway } from '@metadata';
import { ApiProperty } from '@nestjs/swagger';
import {
   BaseSchemaOptions,
   BooleanSchemaOptions,
   IsSchemaType,
   NumberSchemaOptions,
   PasswordSchemaOptions,
   StringSchemaOptions,
} from '../type/schema';

const parseOptionsSchema = (
   type: 'string' | 'number' | 'integer' | 'boolean' | 'object',
   options?: BaseSchemaOptions,
): PropertyOptions<IsSchemaType> => {
   const propsOptions: PropertyOptions<IsSchemaType> = {};
   const schema: ObjectRecord = {};

   if (options?.optional) {
      propsOptions.optional = true;
   }

   if (Is.undefined(options?.nullable)) {
      propsOptions.nullable = propsOptions.optional === true;
   } else {
      propsOptions.nullable = !!options.nullable;
   }

   if (options?.each) {
      schema.type = 'array';
      schema.items = { type: propsOptions.nullable ? [type, 'null'] : type };

      if (options.each === 'unique') {
         propsOptions.validate = [{ is: 'arrayUnique', each: true }];
      }
   } else {
      schema.type = propsOptions.nullable ? [type, 'null'] : type;
   }

   propsOptions.schema = schema;

   return propsOptions;
};

export function PropertySchema<IsType extends IsSchemaType | [ClassConstructor<any>]>(
   options?: PropertyOptions<IsType>,
): PropertyDecorator {
   const decorators = [
      (target: Object, propertyKey: PropertyKey): void => {
         if (!target.hasOwnProperty(CLASS_PROPERTIES)) {
            target[CLASS_PROPERTIES] = {};
         }

         target[CLASS_PROPERTIES][propertyKey] = options ?? null;
      },
   ];

   if (options?.swagger !== false && isGateway()) {
      decorators.push(ApiProperty({ ...(options?.swagger || {}), required: options?.optional !== true }));
   }

   return applyDecorators(...decorators);
}

export function StringSchema(options?: StringSchemaOptions) {
   const each = !!options?.each;
   const propsOptions = parseOptionsSchema('string', options);
   propsOptions.validate = (propsOptions.validate || []) as any[];
   propsOptions.validate.push({ is: 'string', each });

   if (options?.format) {
      if (Is.regex(options.format)) {
         propsOptions.validate[0].is = 'matched';
         propsOptions.validate[0].meta = options.format;
      } else {
         if (['date-time', 'email'].includes(options.format)) {
            propsOptions.schema.format = options.format;
         }

         if (options.format === 'date-time') {
            propsOptions.validate[0].is = 'dateString';
         } else {
            propsOptions.validate[0].is = options.format;
         }
      }
   }

   if (options?.equalsTo) {
      propsOptions.validate.push({ is: 'equals', meta: { equalsTo: options.equalsTo }, each });
   }

   if (options?.transform) {
      switch (options.transform) {
         case 'safeHtml':
            propsOptions.transform = { fromType: ['string'], toType: ['toSafeHtml', 'trim'] };
            break;

         case 'trim':
            propsOptions.transform = { fromType: ['string'], toType: ['trim'] };
            break;
      }
   } else if (options?.transform !== false) {
      propsOptions.transform = { fromType: ['string'], toType: ['toStripTags', 'trim'] };
   }

   if (Is.number(options?.minLength)) {
      propsOptions.validate.push({ is: 'minLength', meta: options.minLength, each });
      propsOptions.schema.minLength = options.minLength;
   }

   if (Is.number(options?.maxLength)) {
      propsOptions.validate.push({ is: 'maxLength', meta: options.maxLength, each });
      propsOptions.schema.maxLength = options.maxLength;
   }

   if (options?.notEmpty) {
      propsOptions.validate.push({ is: 'empty', not: true, each });
      propsOptions.schema.required = true;
   }

   if (options?.swagger !== false) {
      propsOptions.swagger = { type: each ? [String] : String, ...(options?.swagger ?? {}) };
   }

   return applyDecorators(PropertySchema(propsOptions));
}

export function NumberSchema(options?: NumberSchemaOptions) {
   const each = !!options?.each;
   const propsOptions = parseOptionsSchema(options?.integer ? 'integer' : 'number', options);
   propsOptions.validate = (propsOptions.validate || []) as any[];
   propsOptions.validate.push({ is: options?.integer ? 'int' : 'number', each });

   if (options.fromString) {
      propsOptions.transform = { fromType: 'string', toType: 'toBoolean' };
   }

   if (Is.number(options?.min)) {
      propsOptions.validate.push({ is: 'min', meta: options.min, each });
      propsOptions.schema.minimum = options.min;
   }

   if (Is.number(options?.max)) {
      propsOptions.validate.push({ is: 'max', meta: options.max, each });
      propsOptions.schema.maximum = options.max;
   }

   if (options?.swagger !== false) {
      propsOptions.swagger = { type: each ? [Number] : Number, ...(options?.swagger ?? {}) };
   }

   return applyDecorators(PropertySchema(propsOptions));
}

export function BooleanSchema(options?: BooleanSchemaOptions) {
   const each = !!options?.each;
   const propsOptions = parseOptionsSchema('boolean', options);
   propsOptions.validate = (propsOptions.validate || []) as any[];
   propsOptions.validate.push({ is: 'boolean', each });

   if (options.fromString) {
      propsOptions.transform = { fromType: 'string', toType: 'toBoolean' };
   }

   if (options?.swagger !== false) {
      propsOptions.swagger = { type: each ? [Number] : Number, ...(options?.swagger ?? {}) };
   }

   return applyDecorators(PropertySchema(propsOptions));
}

export function ObjectSchema(EntityOrDTOClass: ClassConstructor<any>, options?: BaseSchemaOptions) {
   const each = !!options?.each;
   const propsOptions = parseOptionsSchema('object', options);
   propsOptions.validate = (propsOptions.validate || []) as any[];
   propsOptions.validate.push({ is: EntityOrDTOClass, each });
   const properties = {};
   const parseObjectSchema = (ClassRef: ClassConstructor<any>, properties: ObjectRecord) => {
      const props = ClassRef.prototype[CLASS_PROPERTIES] || {};

      for (const prop in props) {
         const opt = props[prop];
         const ref = Is.array(opt.validate) ? opt.validate[0] : opt.validate;
         const isEntityOrDTO = Is.class(ref);

         if (isEntityOrDTO) {
            if (opt.each) {
               properties[prop] = { type: 'array', items: { type: 'object', properties } };
               parseObjectSchema(ref, properties[prop].items.properties);
            } else {
               properties[prop] = { type: 'object', properties: {} };
               parseObjectSchema(ref, properties[prop].properties);
            }
         } else {
            properties[prop] = props[prop].schema;
         }
      }
   };

   parseObjectSchema(EntityOrDTOClass, properties);

   if (propsOptions.schema.items) {
      propsOptions.schema.items = properties;
   } else {
      propsOptions.schema.properties = properties;
   }

   if (options?.swagger !== false) {
      propsOptions.swagger = { type: each ? [EntityOrDTOClass] : EntityOrDTOClass, ...(options?.swagger ?? {}) };
   }

   return applyDecorators(PropertySchema(propsOptions));
}

export function EnumSchema(
   enumArray: Array<string | number | boolean | null>,
   options?: Omit<BaseSchemaOptions, 'nullable'>,
) {
   const each = !!options?.each;
   const propsOptions = parseOptionsSchema('object', options);
   propsOptions.validate = (propsOptions.validate || []) as any[];
   propsOptions.validate.push({ is: 'inArray', meta: enumArray, each });
   const schema: ObjectRecord = { type: [], enum: enumArray };
   enumArray.forEach((el) => {
      if (!schema.includes(typeof el)) {
         schema.type.push(typeof el);
      }
   });

   if (propsOptions.schema.items) {
      propsOptions.schema.items = schema;
   } else {
      propsOptions.schema.properties = schema;
   }

   if (options?.swagger !== false) {
      propsOptions.swagger = { enum: each ? [enumArray] : enumArray, ...(options?.swagger ?? {}) };
   }

   return applyDecorators(PropertySchema(propsOptions));
}

export function PasswordSchema(options?: PasswordSchemaOptions) {
   const minLength = options?.minLength ?? 8;
   const noSpaces = options?.noSpaces ?? true;
   const minSpecialChars = options?.minSpecialChars ?? 1;
   const minUpper = options?.minUpper ?? 1;
   const minLower = options?.minLower ?? 1;
   const minNumber = options?.minNumber ?? 1;
   const each = !!options?.each;
   const propsOptions = parseOptionsSchema('object', options);
   propsOptions.validate = (propsOptions.validate || []) as any[];
   propsOptions.validate.push({
      is: 'strongPassword' as IsSchemaType,
      meta: { minLength, minLower, minNumber, minSpecialChars, minUpper, noSpaces },
      each,
   });

   if (options?.equalsTo) {
      propsOptions.validate.push({ is: 'equals', meta: { equalsTo: options.equalsTo }, each });
   }

   if (options?.swagger !== false) {
      propsOptions.swagger = { enum: each ? [String] : String, ...(options?.swagger ?? {}) };
   }

   return applyDecorators(PropertySchema(propsOptions));
}

export function JsonSchema(options?: BaseSchemaOptions) {
   const each = !!options?.each;
   const propsOptions = parseOptionsSchema('object', options);
   propsOptions.validate = (propsOptions.validate || []) as any[];
   propsOptions.validate.push({ is: 'flatObject', each });

   if (options?.swagger !== false) {
      propsOptions.swagger = { enum: each ? [Object] : Object, ...(options?.swagger ?? {}) };
   }

   return applyDecorators(PropertySchema(propsOptions));
}
