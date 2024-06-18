import { ObjectRecord } from '@mvanvu/ujs';
import { ClassConstructor } from '../type';
import { CLASS_PROPERTIES } from '../constant';

export class BaseEntity<TEntity extends object = ObjectRecord> {
   constructor(entity?: TEntity) {
      if (entity) {
         this.bind(entity);
      }
   }

   static bindToClass<T>(ClassRef: ClassConstructor<T>, obj: ObjectRecord): T {
      const props: string[] = Object.keys(ClassRef.prototype[CLASS_PROPERTIES] || {});
      const entity = new ClassRef();

      for (const prop of props) {
         entity[prop] = obj[prop] ?? undefined;
      }

      return entity;
   }

   bind(entity: ObjectRecord): this {
      Object.assign(this, BaseEntity.bindToClass(this.constructor as ClassConstructor<typeof this>, entity));

      return this;
   }
}
