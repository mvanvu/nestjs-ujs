import { CommonType, Transform } from '@mvanvu/ujs';

export const TRANSFORMER_PROP = '__TRANSFORMER_PROPS__';

export type TransformType<T = keyof typeof Transform> = T extends 'clean' | 'cleanIfType' | 'prototype' ? never : T;

export type TransformOptions = {
   toType: TransformType | TransformType[];
   fromType?: CommonType | CommonType[];
};

export function Transformer(options: TransformOptions): PropertyDecorator {
   return (target: Object, propertyKey: PropertyKey): void => {
      type PropType = Record<PropertyKey, TransformOptions[]>;

      if (!target.hasOwnProperty(TRANSFORMER_PROP)) {
         target[TRANSFORMER_PROP] = {} as PropType;
      }

      if (!target[TRANSFORMER_PROP][propertyKey]) {
         target[TRANSFORMER_PROP][propertyKey] = [];
      }

      (target[TRANSFORMER_PROP] as PropType)[propertyKey].push(options);
   };
}
