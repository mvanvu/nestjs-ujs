import { applyDecorators } from '@nestjs/common';
import { Property } from './property';
import { ClassConstructor, Is, IsValidType, ObjectRecord } from '@mvanvu/ujs';
import { PropertyOptions, SwaggerOptions, ValidationOptions } from '../type/common';
import { CLASS_PROPERTIES } from '@shared-library/constant';
type Each = boolean | 'unique';
interface BaseSchemaOptions {
   each?: Each;
   optional?: boolean;
   nullable?: boolean;
   swagger?: SwaggerOptions;
}

interface StringSchemaOptions extends BaseSchemaOptions {
   format?: 'email' | 'url' | 'ipV4' | 'creditCard' | 'date-time' | 'mongoId';
   transform?: 'safeHtml' | 'trim' | false;
   minLength?: number;
   maxLength?: number;
   notEmpty?: boolean;
}

interface NumberSchemaOptions extends BaseSchemaOptions {
   integer?: boolean;
   min?: number;
   max?: number;
   fromString?: boolean;
}

interface BooleanSchemaOptions extends BaseSchemaOptions {
   fromString?: boolean;
}

const parseEachSchema = (
   type: 'string' | 'number' | 'integer' | 'boolean',
   each?: boolean,
   nullable?: boolean,
): ObjectRecord => {
   const schema: ObjectRecord = {};

   if (each) {
      schema.type = 'array';
      schema.items = { type: nullable ? [type, 'null'] : type };
   } else {
      schema.type = nullable ? [type, 'null'] : type;
   }

   return schema;
};

export function StringSchema(options?: StringSchemaOptions) {
   const each = !!options?.each;
   const propsOptions: PropertyOptions<IsValidType> = {};
   const validate: Array<ValidationOptions<IsValidType>> = [{ is: 'string', each }];
   const schema = parseEachSchema('string', each, options?.nullable);

   if (options?.format) {
      if (['date-time', 'email'].includes(options.format)) {
         schema.format = options.format;
      }

      if (options.format === 'date-time') {
         validate[0].is = 'dateString';
      } else {
         validate[0].is = options.format;
      }
   }

   if (options?.each === 'unique') {
      validate.push({ is: 'arrayUnique', each });
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
      validate.push({ is: 'minLength', meta: options.minLength, each });
      schema.minLength = options.minLength;
   }

   if (Is.number(options?.maxLength)) {
      validate.push({ is: 'maxLength', meta: options.maxLength, each });
      schema.maxLength = options.maxLength;
   }

   if (options?.notEmpty) {
      validate.push({ is: 'empty', not: true, each });
      schema.required = true;
   }

   propsOptions.validate = validate;
   propsOptions.schema = schema;

   if (options?.swagger?.disabled !== false) {
      propsOptions.swagger = { ...(options?.swagger ?? {}), type: each ? [String] : String };
   }

   return applyDecorators(Property(propsOptions));
}

export function NumberSchema(options?: NumberSchemaOptions) {
   const each = !!options?.each;
   const propsOptions: PropertyOptions<IsValidType> = {};
   const validate: Array<ValidationOptions<IsValidType>> = [{ is: options?.integer ? 'int' : 'number', each }];
   const schema = parseEachSchema(options?.integer ? 'integer' : 'number', each, options?.nullable);

   if (options.fromString) {
      propsOptions.transform = { fromType: 'string', toType: 'toBoolean' };
   }

   if (options?.each === 'unique') {
      validate.push({ is: 'arrayUnique', each });
   }

   if (Is.number(options?.min)) {
      validate.push({ is: 'min', meta: options.min, each });
      schema.minimum = options.min;
   }

   if (Is.number(options?.max)) {
      validate.push({ is: 'max', meta: options.max, each });
      schema.maximum = options.max;
   }

   propsOptions.validate = validate;
   propsOptions.schema = schema;

   if (options?.swagger?.disabled !== false) {
      propsOptions.swagger = { ...(options?.swagger ?? {}), type: each ? [Number] : Number };
   }

   return applyDecorators(Property(propsOptions));
}

export function BooleanSchema(options?: BooleanSchemaOptions) {
   const each = !!options?.each;
   const propsOptions: PropertyOptions<IsValidType> = {};
   const validate: Array<ValidationOptions<IsValidType>> = [{ is: 'boolean', each }];
   const schema = parseEachSchema('boolean', each, options?.nullable);

   if (options.fromString) {
      propsOptions.transform = { fromType: 'string', toType: 'toBoolean' };
   }

   if (options?.each === 'unique') {
      validate.push({ is: 'arrayUnique', each });
   }

   propsOptions.validate = validate;
   propsOptions.schema = schema;

   if (options?.swagger?.disabled !== false) {
      propsOptions.swagger = { ...(options?.swagger ?? {}), type: each ? [Number] : Number };
   }

   return applyDecorators(Property(propsOptions));
}

export function ObjectSchema(EntityOrDTOClass: ClassConstructor<any>, options?: BaseSchemaOptions) {
   const schema: ObjectRecord = { type: 'object', properties: {} };
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

   parseObjectSchema(EntityOrDTOClass, schema.properties);
   type IsType = IsValidType | ClassConstructor<any>;
   const each = !!options?.each;
   const propsOptions: PropertyOptions<IsType> = {};
   const validate: Array<ValidationOptions<IsType>> = [{ is: EntityOrDTOClass, each }];

   if (options?.each === 'unique') {
      validate.push({ is: 'arrayUnique', each });
   }

   propsOptions.validate = validate;
   propsOptions.schema = each ? { type: 'array', items: schema } : schema;

   if (options?.swagger?.disabled !== false) {
      propsOptions.swagger = { ...(options?.swagger ?? {}), type: each ? [EntityOrDTOClass] : EntityOrDTOClass };
   }

   return applyDecorators(Property(propsOptions));
}

export function EnumSchema(
   enumArray: Array<string | number | boolean | null>,
   options?: Omit<BaseSchemaOptions, 'nullable'>,
) {
   const each = !!options?.each;
   const propsOptions: PropertyOptions<IsValidType> = {};
   const validate: Array<ValidationOptions<IsValidType>> = [{ is: 'inArray', meta: enumArray, each }];

   if (options?.each === 'unique') {
      validate.push({ is: 'arrayUnique', each });
   }

   const schema: ObjectRecord = { type: [], enum: enumArray };
   enumArray.forEach((el) => {
      if (!schema.includes(typeof el)) {
         schema.type.push(typeof el);
      }
   });

   propsOptions.validate = validate;
   propsOptions.schema = each ? { type: 'array', items: schema } : schema;

   if (options?.swagger?.disabled !== false) {
      propsOptions.swagger = { ...(options?.swagger ?? {}), enum: each ? [enumArray] : enumArray };
   }

   return applyDecorators(Property(propsOptions));
}
