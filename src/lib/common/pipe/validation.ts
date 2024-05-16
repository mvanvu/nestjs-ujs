import { ArgumentMetadata, Injectable, PipeTransform } from '@nestjs/common';
import { EqualsRulesOptions, Is, IsValidType, ObjectRecord, Transform, Util } from '@mvanvu/ujs';
import { CLASS_PROPERTIES, ClassConstructor, PropertyOptions, ThrowException } from '@lib/common';

export async function validateDTO(data: ObjectRecord, DTOClassRef: ClassConstructor<any>, thisInstance?: any) {
   if (!Is.object(data) || !Is.class(DTOClassRef)) {
      return data;
   }

   if (!DTOClassRef.prototype['__INIT_PARENT_PROPERTIES__']) {
      DTOClassRef.prototype['__INIT_PARENT_PROPERTIES__'] = true;
      let parentClass = Object.getPrototypeOf(DTOClassRef);

      while (Is.class(parentClass)) {
         if (parentClass.prototype[CLASS_PROPERTIES]) {
            Object.assign(DTOClassRef.prototype[CLASS_PROPERTIES], parentClass.prototype[CLASS_PROPERTIES]);
         }

         parentClass = Object.getPrototypeOf(parentClass);
      }
   }

   const error: Record<string, Array<string | number>> = {};
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
         for (const validateOption of Array.isArray(propOptions.validate)
            ? propOptions.validate
            : [propOptions.validate]) {
            const { is: type } = validateOption;
            let isValid: boolean;

            if (Is.callable(type)) {
               isValid = !!(await Util.callAsync(thisInstance, type, val));
            } else if (type === 'equals') {
               const { equalsTo } = (validateOption.meta as EqualsRulesOptions) || {};
               isValid = typeof equalsTo === 'string' && Is.equals(val, data[equalsTo]);
            } else {
               isValid = Is.valid(val, { type, each: validateOption.each, meta: validateOption.meta as any });
            }

            const not = validateOption.not === true;

            if (not) {
               isValid = !isValid;
            }

            if (!isValid) {
               if (!error[prop]) {
                  error[prop] = [];
               }

               error[prop].push(
                  Is.nullOrUndefined(validateOption.code)
                     ? `${not ? 'NOT_' : ''}IS_${Util.camelToSnackCase(type).toUpperCase()}`
                     : <string | number>validateOption.code,
               );
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
