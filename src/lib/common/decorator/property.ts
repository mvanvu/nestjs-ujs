import { PropertyOptions } from '../type';
import { CLASS_PROPERTIES } from '../constant';
import { IsValidType } from '@mvanvu/ujs';

export function EntityProperty<IsType extends IsValidType>(options?: PropertyOptions<IsType>): PropertyDecorator {
   return (target: Object, propertyKey: PropertyKey): void => {
      if (!target.hasOwnProperty(CLASS_PROPERTIES)) {
         target[CLASS_PROPERTIES] = {};
      }

      target[CLASS_PROPERTIES][propertyKey] = options;
   };
}
