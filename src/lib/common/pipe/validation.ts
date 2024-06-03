import { ArgumentMetadata, HttpException, Injectable, PipeTransform } from '@nestjs/common';
import { EqualsRulesOptions, Is, IsValidType, ObjectRecord, Transform, Util } from '@mvanvu/ujs';
import { ClassConstructor, PropertyOptions, ValidationCode, ValidationOptions } from '../type/common';
import { initParentProperties } from '../entity/mapped-type';
import { CLASS_PROPERTIES } from '../constant';
import { ThrowException } from '../exception/throw';
import { RpcException } from '@nestjs/microservices';

export async function validateDTO(data: ObjectRecord, DTOClassRef: ClassConstructor<any>) {
   if (!Is.object(data) || !Is.class(DTOClassRef)) {
      return data;
   }

   initParentProperties(DTOClassRef);
   const error: Record<string, Array<string | number | ObjectRecord>> = {};
   const propertyOptions: Record<string, PropertyOptions<IsValidType>> | undefined =
      DTOClassRef.prototype[CLASS_PROPERTIES];
   const props: string[] = Object.keys(DTOClassRef.prototype[CLASS_PROPERTIES] || {});

   // Cleanup data
   for (const prop in data) {
      if (!props.includes(prop)) {
         delete data[prop];
      }
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
      const val: any = data[prop];
      const propOptions = propertyOptions[prop];

      if (!propOptions || (propOptions.optional === true && Is.nullOrUndefined(val))) {
         if (Is.undefined(val)) {
            delete data[prop];
         }

         continue;
      }

      if (propOptions.validate) {
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
                  isValid = Is.valid(val, { type: <IsValidType>validateIsType, each, meta });
               }
            }

            if (not === true) {
               isValid = !isValid;
            }

            if (Is.nothing(errorCode)) {
               if (code) {
                  errorCode = code;
               } else if (Is.string(validateIsType)) {
                  errorCode = `${not ? 'NOT_' : ''}IS_${Util.camelToSnackCase(validateIsType).toUpperCase()}`;
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

   new ThrowException(error);
}

@Injectable()
export class ValidationPipe implements PipeTransform {
   async transform(value: ObjectRecord, meta: ArgumentMetadata) {
      const { metatype: ClassContructor } = meta;

      return await validateDTO(value, ClassContructor);
   }
}
