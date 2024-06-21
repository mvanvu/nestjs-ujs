import { Is, ObjectRecord, Util } from '@mvanvu/ujs';
import { Type } from '@nestjs/common';
import { ClassConstructor } from '../type';
import { CLASS_PROPERTIES } from '../constant';

export function collectAllProperties(ClassRef: ClassConstructor<any>): ObjectRecord {
   const props: ObjectRecord = {};
   let parentClass = Object.getPrototypeOf(ClassRef);

   while (Is.class(parentClass)) {
      if (parentClass.prototype[CLASS_PROPERTIES]) {
         const properties = Util.clone(parentClass.prototype[CLASS_PROPERTIES]);
         for (const key in properties) {
            if (!props[key]) {
               props[key] = properties[key];
            }
         }
      }

      parentClass = Object.getPrototypeOf(parentClass);
   }

   return props;
}

export function setClassProperties(
   ClassRef: ClassConstructor<any>,
   options?: { includeKeys?: string[]; excludeKeys?: string[]; optional?: boolean },
): void {
   ClassRef.prototype[CLASS_PROPERTIES] = {};
   const properties = collectAllProperties(ClassRef);

   if (options?.includeKeys?.length) {
      for (const key of options.includeKeys) {
         if (properties.hasOwnProperty(key)) {
            ClassRef.prototype[CLASS_PROPERTIES][key] = properties[key];
         }
      }
   } else if (options?.excludeKeys?.length) {
      for (const key in properties) {
         if (!options.excludeKeys.includes(key)) {
            ClassRef.prototype[CLASS_PROPERTIES][key] = properties[key];
         }
      }
   } else if (options?.optional) {
      for (const key in properties) {
         ClassRef.prototype[CLASS_PROPERTIES][key] = { ...properties[key], optional: true };
      }
   }
}

export function IPickType<T, K extends keyof T>(
   ClassRef: Type<T>,
   keys: readonly K[],
): Type<Pick<T, (typeof keys)[number]>> {
   class PickedClass extends (ClassRef as ClassConstructor<any>) {}

   setClassProperties(PickedClass, { includeKeys: keys as unknown as string[] });
   Object.defineProperty(PickedClass, 'name', { value: `Picked${ClassRef.constructor.name}` });

   return PickedClass as Type<Pick<T, (typeof keys)[number]>>;
}

export function IPartialType<T>(ClassRef: Type<T>): Type<Partial<T>> {
   class PartialClass extends (ClassRef as ClassConstructor<any>) {}

   setClassProperties(PartialClass, { optional: true });
   Object.defineProperty(PartialClass, 'name', { value: `Partial${ClassRef.constructor.name}` });

   return PartialClass as Type<Partial<T>>;
}

export function IOmitType<T, K extends keyof T>(
   ClassRef: Type<T>,
   keys: readonly K[],
): Type<Omit<T, (typeof keys)[number]>> {
   class OmitClass extends (ClassRef as ClassConstructor<any>) {}
   setClassProperties(OmitClass, { excludeKeys: keys as unknown as string[] });
   Object.defineProperty(OmitClass, 'name', { value: `Omit${ClassRef.constructor.name}` });

   return OmitClass as Type<Omit<T, (typeof keys)[number]>>;
}
