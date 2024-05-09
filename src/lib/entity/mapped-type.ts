import { CLASS_PROPERTIES, DTO_FOR } from '@lib/decorator';
import { PropertyOptions } from '@lib/type';
import { Type } from '@nestjs/common';
import { PickType, PartialType, OmitType } from '@nestjs/swagger';

export function IPickType<T, K extends keyof T>(
   classRef: Type<T>,
   keys: readonly K[],
): Type<Pick<T, (typeof keys)[number]>> {
   const PickedClass = PickType(classRef, keys);
   PickedClass.prototype[DTO_FOR] = classRef.prototype[DTO_FOR];
   const props = classRef.prototype[CLASS_PROPERTIES] as Record<string, PropertyOptions<any>>;

   if (props) {
      PickedClass.prototype[CLASS_PROPERTIES] = {};

      for (const key of keys) {
         if (props.hasOwnProperty(key)) {
            PickedClass.prototype[CLASS_PROPERTIES][key] = props[key as string];
         }
      }
   }

   return PickedClass;
}

export function IPartialType<T>(classRef: Type<T>, options?: { skipNullProperties?: boolean }): Type<Partial<T>> {
   const PartialClass = PartialType(classRef, options);
   PartialClass.prototype[DTO_FOR] = classRef.prototype[DTO_FOR];
   const props = classRef.prototype[CLASS_PROPERTIES] as Record<string, PropertyOptions<any>>;

   if (props) {
      PartialClass.prototype[CLASS_PROPERTIES] = {};

      for (const prop in props) {
         PartialClass.prototype[CLASS_PROPERTIES][prop] = { ...props[prop], optional: true };
      }
   }

   return PartialClass;
}

export function IOmitType<T, K extends keyof T>(
   classRef: Type<T>,
   keys: readonly K[],
): Type<Omit<T, (typeof keys)[number]>> {
   const OmitClass = OmitType(classRef, keys);
   OmitClass.prototype[DTO_FOR] = classRef.prototype[DTO_FOR];
   const props = classRef.prototype[CLASS_PROPERTIES] as Record<string, PropertyOptions<any>>;

   if (props) {
      OmitClass.prototype[CLASS_PROPERTIES] = {};

      for (const prop in props) {
         if (!(keys as unknown as string[]).includes(prop)) {
            OmitClass.prototype[CLASS_PROPERTIES][prop] = props[prop];
         }
      }
   }

   return OmitClass;
}
