import { ClassConstructor, PropertyOptions } from '../type/common';
import { CLASS_PROPERTIES } from '../constant';
import { IsValidType } from '@mvanvu/ujs';
import { applyDecorators } from '@nestjs/common';
import { ApiProperty } from '@nestjs/swagger';
import { isGateway } from '@metadata';

export function Property<IsType extends IsValidType | ClassConstructor<any> | [ClassConstructor<any>]>(
   options?: PropertyOptions<IsType>,
): PropertyDecorator {
   const decorators = [
      (target: Object, propertyKey: PropertyKey): void => {
         if (!target.hasOwnProperty(CLASS_PROPERTIES)) {
            target[CLASS_PROPERTIES] = {};
         }

         target[CLASS_PROPERTIES][propertyKey] = options ?? null;
      },
   ];

   if (options?.swagger !== false && isGateway()) {
      decorators.push(ApiProperty({ ...(options?.swagger || {}), required: options?.optional !== true }));
   }

   return applyDecorators(...decorators);
}
