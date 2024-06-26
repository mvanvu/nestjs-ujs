import { ArgumentMetadata, HttpException, Injectable, PipeTransform } from '@nestjs/common';
import { EqualsRulesOptions, Is, IsValidType, ObjectRecord, Transform, Util } from '@mvanvu/ujs';
import { ClassConstructor, PropertyOptions, ValidationCode, ValidationOptions } from '../type/common';
import { CLASS_PROPERTIES } from '../constant';
import { ThrowException } from '../exception/throw';
import { RpcException } from '@nestjs/microservices';

export async function validateDTO(data: ObjectRecord, DTOClassRef: ClassConstructor<any>, whiteList?: boolean) {
   if (!Is.object(data) || !Is.flatObject(data) || !Is.class(DTOClassRef)) {
      return data;
   }

   const error: Record<string, Array<string | number | ObjectRecord>> = {};
   const propertyOptions: Record<string, PropertyOptions<IsValidType>> | undefined =
      DTOClassRef.prototype[CLASS_PROPERTIES];
   const props: string[] = Object.keys(DTOClassRef.prototype[CLASS_PROPERTIES] || {});

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
      ThrowException(`The ${notAcceptedProps.join(', ')} ${notAcceptedProps.length > 1 ? `aren't` : `isn't`} accpeted`);
   }

   // Handle transformer
   for (const prop of props) {
      if (propertyOptions[prop]?.transform) {
         const { fromType, toType } = propertyOptions[prop].transform;
         data[prop] = fromType
            ? Transform.cleanIfType(data[prop], toType, fromType)
            : Transform.clean(data[prop], toType);
      }
   }

   // Handle validator
   for (const prop of props) {
      let val: any = data[prop];
      const propOptions = propertyOptions[prop];

      if (!propOptions || (propOptions.optional === true && Is.nothing(val))) {
         continue;
      }

      // Check has default value
      if (propOptions.optional !== true && Is.nothing(val) && !Is.undefined(propOptions.defaultValue)) {
         val = propOptions.defaultValue;
      }

      if (propOptions?.validate) {
         for (const validateOption of Is.array(propOptions.validate) ? propOptions.validate : [propOptions.validate]) {
            let isValid: boolean = true;
            let errorCode: ValidationCode;

            const {
               is: validateIsType,
               each,
               meta,
               not,
               code,
            } = validateOption as ValidationOptions<IsValidType | ClassConstructor<any>>;

            const isArrayCls = Is.array(validateIsType);

            if ((isArrayCls && Is.class(validateIsType[0])) || Is.class(validateIsType)) {
               const IsTypeCls = (isArrayCls ? validateIsType[0] : validateIsType) as ClassConstructor<any>;

               if (isArrayCls && !Is.array(val)) {
                  errorCode = 'IS_ARRAY';
               } else if (!isArrayCls && !Is.object(val)) {
                  errorCode = 'IS_OBJECT';
               } else {
                  const classValidate = async (value: any) => {
                     try {
                        await validateDTO(value, IsTypeCls);
                     } catch (e) {
                        if (e instanceof RpcException) {
                           errorCode = e.getError();
                        } else if (e instanceof HttpException) {
                           errorCode = e.getResponse()['error'];
                        }

                        if (!errorCode) {
                           errorCode = e.error ?? isArrayCls ? 'IS_ARRAY' : 'IS_OBJECT';
                        }

                        isValid = false;
                     }
                  };

                  if (isArrayCls) {
                     for (const v of val) {
                        await classValidate(v);
                     }
                  } else {
                     await classValidate(val);
                  }
               }
            } else {
               if (Is.callable(validateIsType)) {
                  isValid = !!(await Util.callAsync(null, validateIsType, val));
               } else if (validateIsType === 'equals') {
                  const { equalsTo } = (meta as EqualsRulesOptions) || {};
                  isValid = typeof equalsTo === 'string' && Is.equals(val, data[equalsTo]);
               } else {
                  isValid = Is.valid(val, { rule: validateIsType, each, meta });
               }
            }

            if (not === true) {
               isValid = !isValid;
            }

            if (Is.nothing(errorCode)) {
               if (code) {
                  errorCode = code;
               } else if (Is.string(validateIsType)) {
                  errorCode = `IS_${not ? 'NOT_' : ''}${Util.camelToSnackCase(validateIsType).toUpperCase()}`;
               } else {
                  errorCode = 'UNKNOWN';
               }
            }

            if (!isValid) {
               if (!error[prop]) {
                  error[prop] = [];
               }

               error[prop].push({ code: errorCode, meta });
            }
         }
      }
   }

   if (Is.emptyObject(error)) {
      return data;
   }

   const cleanedError: ObjectRecord = {};
   const cleanErrorCode = (err: any, prop?: string): void => {
      if (Is.array(err)) {
         for (const deepError of err) {
            cleanErrorCode(deepError, prop);
         }
      } else if (Is.object(err)) {
         const { code, meta } = err;
         if (Is.nothing(code)) {
            for (const k in err) {
               cleanErrorCode(err[k], prop ? `${prop}.${k}` : k);
            }
         } else if (Is.object(code)) {
            for (const k in code) {
               cleanErrorCode(code[k], prop ? `${prop}.${k}` : k);
            }
         } else if (Is.string(code) && prop) {
            if (!cleanedError[prop]) {
               cleanedError[prop] = [];
            }

            cleanedError[prop].push({ code, meta: meta ?? null });
         }
      }
   };

   // Cleanup error code
   cleanErrorCode(error);

   new ThrowException(cleanedError);
}

@Injectable()
export class ValidationPipe implements PipeTransform {
   constructor(private readonly options?: { whiteList?: boolean }) {}

   async transform(value: ObjectRecord, meta: ArgumentMetadata) {
      const { metatype: ClassContructor } = meta;

      return await validateDTO(value, ClassContructor, this.options?.whiteList);
   }
}
