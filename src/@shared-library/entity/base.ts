import { BaseSchema, ClassConstructor, Is, ObjectSchema, UJS_CLASS_PROPERTIES, Util } from '@mvanvu/ujs';

export class BaseEntity {
   static bindToClass<T>(data: any, ClassRef: ClassConstructor<T>): T {
      const entity = new ClassRef();

      if (Is.object(data)) {
         const props = Reflect.getMetadata(UJS_CLASS_PROPERTIES, ClassRef.prototype) || {};
         const cloneData = Util.clone(data);

         for (const prop in props) {
            const schema = (props[prop] as BaseSchema)?.clone()?.default(undefined);
            const nothing = cloneData[prop] === undefined;

            if (schema instanceof ObjectSchema) {
               schema.whiteList('deep');
            }

            if (nothing || !schema?.check(cloneData[prop])) {
               if (nothing && schema?.isNullable()) {
                  entity[prop] = null;
               }

               continue;
            }

            entity[prop] = schema.getValue();
         }

         if (Is.func(entity['bind'])) {
            Util.call(entity, entity['bind'], data);
         }
      }

      return entity;
   }
}
