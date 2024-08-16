import { BaseSchemaOptions, ClassConstructor } from '../type';
import { validateDTO } from '../pipe/validation';
import { Is, UJS_CLASS_PROPERTIES, Util } from '@mvanvu/ujs';

export class BaseEntity {
   static bindToClass<T>(
      data: any,
      ClassRef: ClassConstructor<T>,
      options?: { validateSchema?: boolean; nullForUndefined?: boolean },
   ): T {
      if (options?.validateSchema === true) {
         validateDTO(data, ClassRef);
      }

      const autoNull = options?.validateSchema !== false;
      const entity = new ClassRef();

      if (Is.object(data)) {
         const props = ClassRef.prototype[UJS_CLASS_PROPERTIES] || {};

         for (const prop in props) {
            if (data[prop] !== undefined) {
               entity[prop] = data[prop];
            }

            if (entity[prop] === undefined && autoNull && !Is.empty(props[prop]?.options)) {
               const { optional, nullable } = props[prop].options as BaseSchemaOptions;

               if (nullable === true || (nullable === undefined && optional === true)) {
                  entity[prop] = null;
               }
            }
         }

         if (Is.func(entity['bind'])) {
            Util.call(entity, entity['bind'], data);
         }
      }

      return entity;
   }
}
