import { ArgumentMetadata, Injectable, PipeTransform } from '@nestjs/common';
import { EqualsRulesOptions, Is, IsValidType, ObjectRecord, Transform, Util } from '@mvanvu/ujs';
import { OPTIONAL_PROP, TRANSFORMER_PROP, TransformOptions, VALIDATION_PROP, ValidationOptions } from '@lib/decorator';

export type ValidationError = Record<string, { code: number | string; message: string }[]>;

@Injectable()
export class ValidationPipe implements PipeTransform {
   async transform(value: ObjectRecord, metadata: ArgumentMetadata) {
      const { metatype: ClassContructor } = metadata;
      const error: ValidationError = {};

      if (Is.object(value) && typeof ClassContructor === 'function') {
         // Handle transformer
         const transformOptions: Record<string, TransformOptions[]> | undefined =
            ClassContructor.prototype[TRANSFORMER_PROP];

         if (transformOptions) {
            for (const prop in transformOptions) {
               if (transformOptions[prop].length) {
                  for (const { fromType, toType } of transformOptions[prop]) {
                     value[prop] = fromType
                        ? Transform.cleanIfType(value[prop], toType, fromType)
                        : Transform.clean(value[prop], toType);
                  }
               }
            }
         }

         // Handle validator
         if (ClassContructor.prototype[VALIDATION_PROP]) {
            const validationProps: Record<string, ValidationOptions<IsValidType>[]> =
               ClassContructor.prototype[VALIDATION_PROP];
            const optionalProps: string[] = ClassContructor.prototype[OPTIONAL_PROP] || [];

            for (const prop in value) {
               const val: any = value[prop];

               if (
                  !validationProps.hasOwnProperty(prop) ||
                  (optionalProps.includes(prop) && [undefined, null].includes(val))
               ) {
                  continue;
               }

               const options = validationProps[prop] as ValidationOptions<IsValidType>[];

               for (const option of options) {
                  const { is: type } = option;
                  let isValid: boolean;

                  if (Is.callable(type)) {
                     isValid = !!(await Util.callAsync(this, type, val));
                  } else if (type === 'equals') {
                     const { equalsTo } = (option.meta as EqualsRulesOptions) || {};
                     isValid = typeof equalsTo === 'string' && Is.equals(val, value[equalsTo]);
                  } else {
                     isValid = Is.valid(val, { type: type, each: option.each, meta: option.meta as any });
                  }

                  if (option.not === true) {
                     isValid = !isValid;
                  }

                  if (isValid) {
                     const code: string | number = Is.nullOrUndefined(option.code) ? 'VALIDATE_FAILED' : option.code;
                     const message: string = Is.nullOrUndefined(option.message) ? 'Bad request' : option.message;
                     error[prop].push({ code, message });
                  }
               }
            }
         }
      }

      if (Is.emptyObject(error)) {
         return value;
      }

      throw error;
   }
}
