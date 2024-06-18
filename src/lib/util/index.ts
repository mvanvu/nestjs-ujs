import { Is, IsValidOptions, IsValidType, Transform } from '@mvanvu/ujs';
import { TransformType } from '../type/common';

export const loadPermissionKeys = (permission?: object | string, permissionKeys?: string[]): string[] => {
   if (!permissionKeys) {
      permissionKeys = [];
   }

   if (typeof permission === 'string') {
      permissionKeys.push(permission);
   } else if (typeof permission === 'object' && permission !== null) {
      for (const prop in permission) {
         loadPermissionKeys(permission[prop], permissionKeys);
      }
   }

   return permissionKeys;
};

export const splitQueryParam = <
   IsType extends IsValidType,
   TValidate extends { rule: IsType | IsType[]; meta?: IsValidOptions<IsType> },
>(
   paramValue: string,
   options?: {
      transform?: TransformType | TransformType[];
      validate?: TValidate | TValidate[];
   },
): string[] | false => {
   const values = paramValue.split(',');

   if (options) {
      for (let i = 0, n = values.length; i < n; i++) {
         if (options.transform) {
            values[i] = Transform.clean(values[i], options.transform);
         }

         if (options.validate) {
            const validateOptions = Is.array(options.validate) ? options.validate : [options.validate];

            for (const validateOption of validateOptions) {
               const rules = Is.array(validateOption.rule) ? validateOption.rule : [validateOption.rule];

               for (const rule of rules) {
                  if (!Is.valid(values[i], { rule, meta: validateOption.meta as any })) {
                     return false;
                  }
               }
            }
         }
      }
   }

   return values;
};
