import { Callable, IsValidType, IsValidOptions } from '@mvanvu/ujs';

export const VALIDATION_PROP: string = '__VALIDATION_PROPS__';

export const OPTIONAL_PROP: string = '__OPTIONAL_PROPS__';

export type ValidationCode = string | number;

export type ValidationOptions<T> = {
   is: T;
   each?: boolean;
   not?: boolean;
   meta?: IsValidOptions<T>['meta'];
   message?: string;
   code?: ValidationCode;
};

export function Validate<T extends IsValidType | Callable>(options: ValidationOptions<T>): PropertyDecorator {
   return (target: Object, propertyKey: PropertyKey): void => {
      if (!target.hasOwnProperty(VALIDATION_PROP)) {
         target[VALIDATION_PROP] = {} as Record<string, ValidationOptions<T>[]>;
      }

      if (!target[VALIDATION_PROP][propertyKey]) {
         target[VALIDATION_PROP][propertyKey] = [];
      }

      (target[VALIDATION_PROP][propertyKey] as ValidationOptions<T>[]).push(options);
   };
}

export function Optional(): PropertyDecorator {
   return (target: Object, propertyKey: PropertyKey): void => {
      if (!target.hasOwnProperty(OPTIONAL_PROP)) {
         target[OPTIONAL_PROP] = [] as string[];
      }

      if (target[OPTIONAL_PROP].includes(propertyKey)) {
         target[OPTIONAL_PROP].push(propertyKey);
      }
   };
}
