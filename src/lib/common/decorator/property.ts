import { PropertyOptions } from '@lib/common/type';
import { IsValidType } from '@mvanvu/ujs';

export const CLASS_PROPERTIES: string = '__CLASS_PROPERTIES__';

export function EntityProperty<IsType extends IsValidType>(options?: PropertyOptions<IsType>): PropertyDecorator {
   return (target: Object, propertyKey: PropertyKey): void => {
      if (!target.hasOwnProperty(CLASS_PROPERTIES)) {
         target[CLASS_PROPERTIES] = {};
      }

      target[CLASS_PROPERTIES][propertyKey] = options;
   };
}
