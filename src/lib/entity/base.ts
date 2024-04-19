import { ENTITY_PROPERTIES } from '@lib/decorator';
import { ClassConstructor } from '@lib/type';
import { ObjectRecord } from '@mvanvu/ujs';

export class BaseEntity {
   static bindToClass<T>(cls: ClassConstructor<T>, obj: ObjectRecord): T {
      const props: string[] = cls.prototype[ENTITY_PROPERTIES] || [];
      const entity = new cls();

      for (const prop in obj) {
         if (props.includes(prop)) {
            entity[prop] = obj[prop];
         }
      }

      delete entity[ENTITY_PROPERTIES];

      return entity;
   }

   bind(obj: ObjectRecord): this {
      Object.assign(this, BaseEntity.bindToClass(this.constructor as ClassConstructor<typeof this>, obj));

      return this;
   }
}
