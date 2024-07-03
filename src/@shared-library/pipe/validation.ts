import { ArgumentMetadata, HttpStatus, Injectable, PipeTransform } from '@nestjs/common';
import { Is, ObjectRecord, Transform } from '@mvanvu/ujs';
import { ClassConstructor } from '../type/common';
import { CLASS_PROPERTIES, INIT_PARENT_PROPERTIES } from '../constant';
import { ThrowException } from '../exception/throw';
import { collectAllProperties } from '@shared-library/entity/mapped-type';
import {
   BaseSchemaOptions,
   BooleanSchemaOptions,
   ClassSchemaOptions,
   EnumSchemaOptions,
   JsonSchemaOptions,
   NumberSchemaOptions,
   PasswordSchemaOptions,
   StringSchemaOptions,
   ValidSchema,
} from '@shared-library/type/schema';

export function validateDTO(data: any, DTOClassRef: ClassConstructor<any>, whiteList?: boolean): ObjectRecord {
   if (!Is.class(DTOClassRef)) {
      ThrowException(`${DTOClassRef} must be a valid DTO class constructor`, HttpStatus.NOT_IMPLEMENTED);
   }

   const errors: { [key: string]: { code: string; message?: string; meta?: any }[] } = {};
   const appendError = (pathKey: string, code: string, message?: string, meta?: any) => {
      if (!errors[pathKey]) {
         errors[pathKey] = [];
      }

      errors[pathKey].push({ code, message: message ?? null, meta: meta ?? null });
   };

   const handleValidate = (dataValue: any, dtoRef: ClassConstructor<any>, path?: string) => {
      if (!Is.json(dataValue)) {
         if (path) {
            appendError(path, 'INVALID_JSON_DATA', 'Must be a jsob object data');
            return;
         }

         ThrowException('The body data must be a jsob object data');
      }

      const propertyOptions =
         dtoRef[INIT_PARENT_PROPERTIES] === true
            ? dtoRef.prototype[CLASS_PROPERTIES] || {}
            : collectAllProperties(dtoRef);
      const props: string[] = Object.keys(propertyOptions);

      // Cleanup data
      const notAcceptedProps: string[] = [];

      for (const prop in data) {
         if (!props.includes(prop)) {
            if (whiteList !== true) {
               notAcceptedProps.push(prop);
            }

            delete data[prop];
         }
      }

      if (notAcceptedProps.length) {
         const message = `The ${notAcceptedProps.join(', ')} ${notAcceptedProps.length > 1 ? `aren't` : `isn't`} accpeted`;

         if (path) {
            appendError(path, 'NOT_ACCEPTED_DATA', message);
            return;
         }

         ThrowException(message);
      }

      // Handle validator
      for (const property in propertyOptions) {
         const propOptions = propertyOptions[property];

         if (!Is.object(propOptions) || Is.empty(propOptions)) {
            continue;
         }

         const pathKey = path ? `${path}.${property}` : property;
         let propValue: any = dataValue[property];

         for (const schema in propOptions) {
            const schemaOptions = propOptions[schema] as BaseSchemaOptions;
            const optional = schemaOptions.optional === true;
            const nullable = schemaOptions.nullable ?? optional;

            if ((propValue === undefined && optional) || (propValue === null && nullable)) {
               continue;
            }

            switch (schema as ValidSchema) {
               case 'string':
                  const stringSchemaOptions = schemaOptions as StringSchemaOptions;

                  // Check to transform to other type of value
                  if (Is.string(dataValue)) {
                     switch (stringSchemaOptions.transform) {
                        case 'safeHtml':
                           propValue = Transform.trim(Transform.toSafeHtml(propValue));
                           break;

                        case 'format':
                           switch (stringSchemaOptions.format) {
                              case 'number':
                                 propValue = Transform.toNumber(propValue);
                                 break;

                              case 'unsignedNumber':
                                 propValue = Transform.toUNumber(propValue);
                                 break;

                              case 'integer':
                                 propValue = Transform.toInt(propValue);
                                 break;

                              case 'unsignedInteger':
                                 propValue = Transform.toUInt(propValue);
                                 break;

                              case 'boolean':
                                 propValue = Transform.toBoolean(propValue);
                                 break;
                           }

                           break;

                        // No transfrom, allows raw value
                        case false:
                           break;

                        // Defaults to strip all tags
                        default:
                           propValue = Transform.trim(Transform.toStripTags(propValue));
                           break;
                     }
                  }

                  if (propValue === '' && stringSchemaOptions.empty === 'skip') {
                     delete dataValue[property];
                     continue;
                  }

                  if (
                     !Is.string(propValue, {
                        format: stringSchemaOptions.format,
                        isArray: stringSchemaOptions.isArray,
                        minLength: stringSchemaOptions.minLength,
                        maxLength: stringSchemaOptions.maxLength,
                        notEmpty: stringSchemaOptions.empty !== false,
                     })
                  ) {
                     appendError(pathKey, stringSchemaOptions.code || 'IS_STRING', stringSchemaOptions.message);
                  }

                  break;

               case 'boolean':
                  const booleanSchemaOptions = schemaOptions as BooleanSchemaOptions;

                  if (!Is.boolean(propValue, { isArray: booleanSchemaOptions.isArray })) {
                     appendError(pathKey, booleanSchemaOptions.code || 'IS_BOOLEAN', booleanSchemaOptions.message);
                  }

                  break;

               case 'number':
                  const numberSchemaOptions = schemaOptions as NumberSchemaOptions;

                  if (
                     !Is.number(propValue, {
                        isArray: numberSchemaOptions.isArray,
                        integer: numberSchemaOptions.integer,
                        min: numberSchemaOptions.min,
                        max: numberSchemaOptions.max,
                     })
                  ) {
                     appendError(pathKey, numberSchemaOptions.code || 'IS_NUMBER', numberSchemaOptions.message);
                  }

                  break;

               case 'password':
                  const passwordSchemaOptions = schemaOptions as PasswordSchemaOptions;
                  const isValidPassword = Is.strongPassword(propValue, {
                     isArray: passwordSchemaOptions.isArray,
                     minLength: passwordSchemaOptions.minLength,
                     minLower: passwordSchemaOptions.minLower,
                     minUpper: passwordSchemaOptions.minUpper,
                     minNumber: passwordSchemaOptions.minNumber,
                     minSpecialChars: passwordSchemaOptions.minSpecialChars,
                  });

                  if (!isValidPassword) {
                     appendError(pathKey, passwordSchemaOptions.code || 'IS_PASSWORD', passwordSchemaOptions.message);
                  }

                  if (
                     passwordSchemaOptions.equalsTo &&
                     !Is.equals(propValue, dataValue[passwordSchemaOptions.equalsTo])
                  ) {
                     appendError(
                        pathKey,
                        passwordSchemaOptions.code || 'IS_PASSWORD_NOT_EQUALS',
                        passwordSchemaOptions.message,
                     );
                  }

                  break;

               case 'enum':
                  const enumSchemaOptions = schemaOptions as EnumSchemaOptions;

                  if (!Is.enum(propValue, { isArray: enumSchemaOptions.isArray, enumArray: enumSchemaOptions.ref })) {
                     appendError(pathKey, enumSchemaOptions.code || 'IS_ENUM', enumSchemaOptions.message);
                  }

                  break;

               case 'json':
                  const jsonSchemaOptions = schemaOptions as JsonSchemaOptions;

                  if (!Is.json(propValue, { isArray: jsonSchemaOptions.isArray })) {
                     appendError(pathKey, jsonSchemaOptions.code || 'IS_JSON', jsonSchemaOptions.message);
                  }

                  break;

               case 'class':
                  const { isArray, ref, code, message } = schemaOptions as ClassSchemaOptions;

                  if (Is.class(ref) && Is.json(propValue, { isArray })) {
                     if (Is.array(propValue)) {
                        propValue.forEach((dt) => handleValidate(dt, ref, pathKey));
                     } else {
                        handleValidate(propValue, ref, pathKey);
                     }
                  } else {
                     appendError(pathKey, code || 'IS_CLASS', message);
                  }

                  break;
            }
         }
      }
   };

   handleValidate(data, DTOClassRef);

   if (!Is.empty(errors)) {
      ThrowException(errors);
   }

   return data;
}

@Injectable()
export class ValidationPipe implements PipeTransform {
   constructor(private readonly options?: { whiteList?: boolean }) {}

   transform(value: any, meta: ArgumentMetadata): ObjectRecord {
      const { metatype: ClassContructor } = meta;
      return validateDTO(value, ClassContructor, this.options?.whiteList);
   }
}
