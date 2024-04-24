import { CLASS_PROPERTIES } from '@lib/decorator';
import { ClassConstructor } from '@lib/type';
import { ObjectRecord } from '@mvanvu/ujs';

export class BaseEntity {
   constructor(entity?: ObjectRecord) {
      if (entity) {
         this.bind(entity);
      }
   }

   static bindToClass<T>(cls: ClassConstructor<T>, obj: ObjectRecord): T {
      const props: string[] = Object.keys(cls.prototype[CLASS_PROPERTIES] || {});
      const entity = new cls();

      for (const prop of props) {
         entity[prop] = obj.hasOwnProperty(prop) ? obj[prop] : null;
      }

      return entity;
   }

   bind(entity: ObjectRecord): this {
      Object.assign(this, BaseEntity.bindToClass(this.constructor as ClassConstructor<typeof this>, entity));

      return this;
   }
}
