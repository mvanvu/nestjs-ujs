import { PropertyOptions } from '../type';
import { CLASS_PROPERTIES } from '../constant';
import { IsValidType } from '@mvanvu/ujs';
import { applyDecorators } from '@nestjs/common';
import { metadata } from '@lib/metadata';
import { ApiProperty } from '@nestjs/swagger';

export function EntityProperty<IsType extends IsValidType>(options?: PropertyOptions<IsType>): PropertyDecorator {
   const decorators = [
      (target: Object, propertyKey: PropertyKey): void => {
         if (!target.hasOwnProperty(CLASS_PROPERTIES)) {
            target[CLASS_PROPERTIES] = {};
         }

         target[CLASS_PROPERTIES][propertyKey] = options;
      },
   ];

   if (metadata.isGateway() && options?.swagger?.disable !== true) {
      decorators.push(ApiProperty({ ...(options?.swagger || {}) }));
   }

   return applyDecorators(...decorators);
}
