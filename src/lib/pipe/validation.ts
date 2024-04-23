import { ArgumentMetadata, HttpException, HttpStatus, Injectable, PipeTransform } from '@nestjs/common';
import { EqualsRulesOptions, Is, IsValidType, ObjectRecord, Transform, Util } from '@mvanvu/ujs';
import { CLASS_PROPERTIES } from '@lib/decorator';
import { PropertyOptions, ValidationError } from '@lib/type';

@Injectable()
export class ValidationPipe implements PipeTransform {
   async transform(value: ObjectRecord, metadata: ArgumentMetadata) {
      const { metatype: ClassContructor } = metadata;
      const error: ValidationError = {};

      if (Is.object(value) && typeof ClassContructor === 'function') {
         const propertyOptions: Record<string, PropertyOptions<IsValidType>> | undefined =
            ClassContructor.prototype[CLASS_PROPERTIES];
         const props: string[] = Object.keys(ClassContructor.prototype[CLASS_PROPERTIES] || {});

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

                  if (validateOption.not === true) {
                     isValid = !isValid;
                  }

                  if (!isValid) {
                     const code: string | number = Is.nullOrUndefined(validateOption.code)
                        ? 'VALIDATE_FAILED'
                        : validateOption.code;
                     const message: string = Is.nullOrUndefined(validateOption.message)
                        ? 'Bad request'
                        : validateOption.message;

                     if (!error[prop]) {
                        error[prop] = [];
                     }

                     error[prop].push({ code, message });
                  }
               }
            }
         }
      }

      if (Is.emptyObject(error)) {
         return value;
      }

      throw new HttpException(error, HttpStatus.BAD_REQUEST);
   }
}
