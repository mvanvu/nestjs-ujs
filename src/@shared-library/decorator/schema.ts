import { applyDecorators } from '@nestjs/common';
import { ClassConstructor, Util } from '@mvanvu/ujs';
import {
   ClassSchemaOptions,
   EnumSchemaOptions,
   JsonSchemaOptions,
   PropertySchemaOptions,
   ValidSchema,
   BooleanSchemaOptions,
   NumberSchemaOptions,
   PasswordSchemaOptions,
   StringSchemaOptions,
} from '../type/schema';
import { CLASS_PROPERTIES } from '../constant';
import { isGateway } from '@metadata';
import { ApiProperty } from '@nestjs/swagger';

export function PropertySchema<T extends ValidSchema>(
   options?: PropertySchemaOptions<T>,
   schema?: T,
): PropertyDecorator {
   const decorators = [
      (target: Object, propertyKey: PropertyKey): void => {
         if (!target.hasOwnProperty(CLASS_PROPERTIES)) {
            target[CLASS_PROPERTIES] = {};
         }

         if (!target[CLASS_PROPERTIES][propertyKey]) {
            target[CLASS_PROPERTIES][propertyKey] = {};
         }

         const propOptions = Util.clone(options || {});

         if (propOptions.hasOwnProperty('swagger')) {
            delete propOptions['swagger'];
         }

         // Each property has only a schema
         target[CLASS_PROPERTIES][propertyKey] = { schema: schema || 'prop', options: propOptions };
      },
   ];

   if (options?.swagger !== false && isGateway()) {
      const swaggerOptions = { ...(options?.swagger || {}), required: options?.optional !== true };

      if (swaggerOptions.type === undefined) {
         const each = !!options?.isArray;

         switch (schema) {
            case 'string':
            case 'password':
               swaggerOptions.type = each ? [String] : String;
               break;

            case 'number':
               swaggerOptions.type = each ? [Number] : Number;
               break;

            case 'boolean':
               swaggerOptions.type = each ? [Boolean] : Boolean;
               break;

            case 'json':
               swaggerOptions.type = each ? [Object] : Object;
               break;

            case 'enum':
               const { ref: enumArray } = (options || {}) as EnumSchemaOptions;
               swaggerOptions.enum = each ? [enumArray] : enumArray;
               break;

            case 'class':
               const { ref: classRef } = (options || {}) as EnumSchemaOptions | ClassSchemaOptions;
               swaggerOptions.type = each ? [classRef] : classRef;
               break;
         }
      }

      decorators.push(ApiProperty(swaggerOptions));
   }

   return applyDecorators(...decorators);
}

export function StringSchema(options?: StringSchemaOptions): PropertyDecorator {
   return applyDecorators(PropertySchema(options, 'string'));
}

export function NumberSchema(options?: NumberSchemaOptions): PropertyDecorator {
   return applyDecorators(PropertySchema(options, 'number'));
}

export function BooleanSchema(options?: BooleanSchemaOptions): PropertyDecorator {
   return applyDecorators(PropertySchema(options, 'boolean'));
}

export function ClassSchema(ref: ClassConstructor<any>, options?: Omit<ClassSchemaOptions, 'ref'>): PropertyDecorator {
   return applyDecorators(PropertySchema({ ref, ...(options || {}) }, 'class'));
}

export function EnumSchema(ref: any[], options?: Omit<EnumSchemaOptions, 'ref'>): PropertyDecorator {
   return applyDecorators(PropertySchema({ ref, ...(options || {}) }, 'enum'));
}

export function PasswordSchema(options?: PasswordSchemaOptions): PropertyDecorator {
   return applyDecorators(PropertySchema(options, 'password'));
}

export function JsonSchema(options?: JsonSchemaOptions): PropertyDecorator {
   return applyDecorators(PropertySchema(options, 'json'));
}

export function IDSchema(options?: Omit<StringSchemaOptions, 'format'>): PropertyDecorator {
   return applyDecorators(PropertySchema({ ...(options || {}), format: 'mongoId' }, 'string'));
}

export function NameSchema(options?: Omit<StringSchemaOptions, 'empty'>): PropertyDecorator {
   return applyDecorators(PropertySchema({ format: 'trim', ...(options || {}), empty: false }, 'string'));
}

export function EmailSchema(options?: Omit<StringSchemaOptions, 'format'>): PropertyDecorator {
   return applyDecorators(PropertySchema({ ...(options || {}), format: 'email' }, 'string'));
}

export function DateSchema(options?: Omit<StringSchemaOptions, 'format'>): PropertyDecorator {
   return applyDecorators(PropertySchema({ ...(options || {}), format: 'date-time' }, 'string'));
}

export function ImageSchema(options?: Omit<StringSchemaOptions, 'format'>): PropertyDecorator {
   return applyDecorators(PropertySchema({ ...(options || {}), format: 'image' }, 'string'));
}

export function IntSchema(options?: Omit<NumberSchemaOptions, 'integer'>): PropertyDecorator {
   return applyDecorators(PropertySchema({ ...(options || {}), integer: true }, 'number'));
}

export function UIntSchema(options?: Omit<NumberSchemaOptions, 'integer'>): PropertyDecorator {
   return applyDecorators(PropertySchema({ ...(options || {}), integer: true, min: 0 }, 'number'));
}

export function HtmlSchema(options?: Omit<StringSchemaOptions, 'transform'>): PropertyDecorator {
   return applyDecorators(PropertySchema({ ...(options || {}), transform: 'safeHtml' }, 'string'));
}

export function RawSchema(options?: Omit<StringSchemaOptions, 'transform'>): PropertyDecorator {
   return applyDecorators(PropertySchema({ ...(options || {}), transform: false }, 'string'));
}
