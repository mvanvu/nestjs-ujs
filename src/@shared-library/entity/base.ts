import { ClassConstructor } from '../type';
import { CLASS_PROPERTIES } from '../constant';
import { validateDTO } from '../pipe/validation';
import { Is, Util } from '@mvanvu/ujs';

export class BaseEntity {
   static bindToClass<T>(data: any, ClassRef: ClassConstructor<T>, validateSchema?: boolean): T {
      if (validateSchema === true) {
         validateDTO(data, ClassRef);
      }

      const entity = new ClassRef();

      if (Is.object(data)) {
         for (const prop of Object.keys(ClassRef.prototype[CLASS_PROPERTIES] || {})) {
            if (data[prop] !== undefined) {
               entity[prop] = data[prop];
            }
         }

         if (Is.func(entity['bind'])) {
            Util.call(entity, entity['bind']);
         }
      }

      return entity;
   }
}
