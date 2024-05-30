import { Is } from '@mvanvu/ujs';
import { Type } from '@nestjs/common';
import { PickType, PartialType, OmitType } from '@nestjs/swagger';
import { ClassConstructor } from '../type';
import { CLASS_PROPERTIES } from '../constant';

export function initParentProperties(ClassRef: ClassConstructor<any>): void {
   if (!ClassRef.prototype[CLASS_PROPERTIES]) {
      ClassRef.prototype[CLASS_PROPERTIES] = {};
   }

   let parentClass = Object.getPrototypeOf(ClassRef);

   while (Is.class(parentClass)) {
      if (parentClass.prototype[CLASS_PROPERTIES]) {
         Object.assign(ClassRef.prototype[CLASS_PROPERTIES], parentClass.prototype[CLASS_PROPERTIES]);
      }

      parentClass = Object.getPrototypeOf(parentClass);
   }
}

export function assignClassProperties(
   ClassRefA: ClassConstructor<any>,
   ClassRefB: ClassConstructor<any>,
   options?: { includeKeys?: string[]; excludeKeys?: string[]; optional?: boolean },
): void {
   if (!ClassRefA.prototype[CLASS_PROPERTIES]) {
      ClassRefA.prototype[CLASS_PROPERTIES] = {};
   }

   if (!ClassRefB.prototype[CLASS_PROPERTIES]) {
      ClassRefB.prototype[CLASS_PROPERTIES] = {};
   }

   if (options?.includeKeys?.length || options?.excludeKeys?.length) {
      if (options.includeKeys) {
         for (const key of options.includeKeys) {
            if (ClassRefB.prototype[CLASS_PROPERTIES].hasOwnProperty(key)) {
               ClassRefA.prototype[CLASS_PROPERTIES][key] = ClassRefB.prototype[CLASS_PROPERTIES][key];

               if (options.optional) {
                  ClassRefA.prototype[CLASS_PROPERTIES][key].optional = true;
               }
            }
         }
      }

      if (options.excludeKeys) {
         for (const key in ClassRefB.prototype[CLASS_PROPERTIES]) {
            if (ClassRefB.prototype[CLASS_PROPERTIES].hasOwnProperty(key) && !options.excludeKeys.includes(key)) {
               ClassRefA.prototype[CLASS_PROPERTIES][key] = ClassRefB.prototype[CLASS_PROPERTIES][key];

               if (options.optional) {
                  ClassRefA.prototype[CLASS_PROPERTIES][key].optional = true;
               }
            }
         }
      }
   } else {
      Object.assign(ClassRefA.prototype[CLASS_PROPERTIES], ClassRefB.prototype[CLASS_PROPERTIES]);

      if (options.optional) {
         for (const key in ClassRefA.prototype[CLASS_PROPERTIES]) {
            ClassRefA.prototype[CLASS_PROPERTIES][key].optional = true;
         }
      }
   }
}

export function IPickType<T, K extends keyof T>(
   ClassRef: Type<T>,
   keys: readonly K[],
): Type<Pick<T, (typeof keys)[number]>> {
   const PickedClass = PickType(ClassRef, keys);
   initParentProperties(ClassRef);
   assignClassProperties(PickedClass, ClassRef, { includeKeys: keys as unknown as string[] });
   Object.defineProperty(PickedClass, 'name', { value: `Picked${ClassRef.constructor.name}` });

   return PickedClass;
}

export function IPartialType<T>(ClassRef: Type<T>, options?: { skipNullProperties?: boolean }): Type<Partial<T>> {
   const PartialClass = PartialType(ClassRef, options);
   initParentProperties(ClassRef);
   assignClassProperties(PartialClass, ClassRef, { optional: true });
   Object.defineProperty(PartialClass, 'name', { value: `Partial${ClassRef.constructor.name}` });

   return PartialClass;
}

export function IOmitType<T, K extends keyof T>(
   ClassRef: Type<T>,
   keys: readonly K[],
): Type<Omit<T, (typeof keys)[number]>> {
   const OmitClass = OmitType(ClassRef, keys);
   initParentProperties(ClassRef);
   assignClassProperties(OmitClass, ClassRef, { excludeKeys: keys as unknown as string[] });
   Object.defineProperty(OmitClass, 'name', { value: `Omit${ClassRef.constructor.name}` });

   return OmitClass;
}
