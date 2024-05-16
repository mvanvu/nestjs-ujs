import { CLASS_PROPERTIES } from '@lib/common/decorator';
import { ClassConstructor } from '@lib/common/type';
import { Is, ObjectRecord } from '@mvanvu/ujs';

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
         if (!Is.nothing(obj[prop])) {
            entity[prop] = obj[prop];
         }
      }

      return entity;
   }

   bind(entity: ObjectRecord): this {
      Object.assign(this, BaseEntity.bindToClass(this.constructor as ClassConstructor<typeof this>, entity));

      return this;
   }
}
