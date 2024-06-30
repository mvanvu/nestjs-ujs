import { ObjectRecord, Util } from '@mvanvu/ujs';
import { Type } from '@nestjs/common';
import { ClassConstructor } from '../type';
import { CLASS_PROPERTIES, INIT_PARENT_PROPERTIES } from '../constant';
import { OmitType, PartialType, PickType } from '@nestjs/swagger';

export function collectAllProperties(ClassRef: ClassConstructor<any>): ObjectRecord {
   const props: ObjectRecord = {};
   let parentClass = ClassRef;

   while (parentClass) {
      if (parentClass.prototype?.[CLASS_PROPERTIES]) {
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

function setClassProperties(
   ClassRef: ClassConstructor<any>,
   properties: ObjectRecord,
   options: { includeKeys?: string[]; excludeKeys?: string[]; optional?: boolean },
): void {
   ClassRef.prototype[INIT_PARENT_PROPERTIES] = true;
   ClassRef.prototype[CLASS_PROPERTIES] = {};

   if (options.includeKeys?.length) {
      for (const key of options.includeKeys) {
         if (properties.hasOwnProperty(key)) {
            ClassRef.prototype[CLASS_PROPERTIES][key] = properties[key];
         }
      }
   } else if (options.excludeKeys?.length) {
      for (const key in properties) {
         if (!options.excludeKeys.includes(key)) {
            ClassRef.prototype[CLASS_PROPERTIES][key] = properties[key];
         }
      }
   } else if (options.optional) {
      for (const key in properties) {
         ClassRef.prototype[CLASS_PROPERTIES][key] = { ...properties[key], optional: true };
      }
   }
}

export function IPickType<T, K extends keyof T>(
   ClassRef: Type<T>,
   keys: readonly K[],
): Type<Pick<T, (typeof keys)[number]>> {
   const PickedClass = PickType(ClassRef, keys);

   setClassProperties(PickedClass, collectAllProperties(ClassRef), { includeKeys: keys as unknown as string[] });
   Object.defineProperty(PickedClass, 'name', { value: `Picked${ClassRef.constructor.name}` });

   return PickedClass;
}

export function IPartialType<T>(ClassRef: Type<T>): Type<Partial<T>> {
   const PartialClass = PartialType(ClassRef);

   setClassProperties(PartialClass, collectAllProperties(ClassRef), { optional: true });
   Object.defineProperty(PartialClass, 'name', { value: `Partial${ClassRef.constructor.name}` });

   return PartialClass;
}

export function IOmitType<T, K extends keyof T>(
   ClassRef: Type<T>,
   keys: readonly K[],
): Type<Omit<T, (typeof keys)[number]>> {
   const OmitClass = OmitType(ClassRef, keys);
   setClassProperties(OmitClass, collectAllProperties(ClassRef), { excludeKeys: keys as unknown as string[] });
   Object.defineProperty(OmitClass, 'name', { value: `Omit${ClassRef.constructor.name}` });

   return OmitClass;
}
