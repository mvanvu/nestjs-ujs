import { ArgumentMetadata, Injectable, PipeTransform } from '@nestjs/common';
import { EqualsRulesOptions, Is, IsValidType, ObjectRecord, Transform, Util } from '@mvanvu/ujs';
import { CLASS_PROPERTIES, PropertyOptions, ThrowException } from '@lib';

@Injectable()
export class ValidationPipe implements PipeTransform {
   async transform(value: ObjectRecord, meta: ArgumentMetadata) {
      const { metatype: ClassContructor } = meta;
      const error: Record<string, Array<string | number>> = {};

      if (Is.object(value) && typeof ClassContructor === 'function') {
         const propertyOptions: Record<string, PropertyOptions<IsValidType>> | undefined =
            ClassContructor.prototype[CLASS_PROPERTIES];
         const props: string[] = Object.keys(ClassContructor.prototype[CLASS_PROPERTIES] || {});

         // Cleanup data
         for (const prop in value) {
            if (!props.includes(prop)) {
               delete value[prop];
            }
         }

         // Handle transformer
         for (const prop of props) {
            if (propertyOptions[prop]?.transform) {
               const { fromType, toType } = propertyOptions[prop].transform;
               value[prop] = fromType
                  ? Transform.cleanIfType(value[prop], toType, fromType)
                  : Transform.clean(value[prop], toType);
            }
         }

         // Handle validator
         for (const prop of props) {
            const val: any = value[prop];
            const propOptions = propertyOptions[prop];

            if (!propOptions || (propOptions.optional === true && Is.nullOrUndefined(val))) {
               if (Is.undefined(val)) {
                  delete value[prop];
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
                     isValid = !!(await Util.callAsync(this, type, val));
                  } else if (type === 'equals') {
                     const { equalsTo } = (validateOption.meta as EqualsRulesOptions) || {};
                     isValid = typeof equalsTo === 'string' && Is.equals(val, value[equalsTo]);
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
      }

      if (Is.emptyObject(error)) {
         return value;
      }

      new ThrowException(error);
   }
}
