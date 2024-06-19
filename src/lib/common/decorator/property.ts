import { ClassConstructor, PropertyOptions } from '../type';
import { CLASS_PROPERTIES } from '../constant';
import { IsValidType } from '@mvanvu/ujs';
import { applyDecorators } from '@nestjs/common';
import { ApiProperty } from '@nestjs/swagger';

export function Property<IsType extends IsValidType | ClassConstructor<any> | [ClassConstructor<any>]>(
   options?: PropertyOptions<IsType>,
): PropertyDecorator {
   const decorators = [
      (target: Object, propertyKey: PropertyKey): void => {
         if (!target.hasOwnProperty(CLASS_PROPERTIES)) {
            target[CLASS_PROPERTIES] = {};
         }

         target[CLASS_PROPERTIES][propertyKey] = options;
      },
   ];

   if (options?.swagger?.disabled !== true) {
      decorators.push(ApiProperty({ ...(options?.swagger || {}), required: options?.optional !== true }));
   }

   return applyDecorators(...decorators);
}
