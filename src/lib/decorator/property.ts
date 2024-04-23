import { PropertyOptions } from '@lib/type';
import { IsValidType } from '@mvanvu/ujs';
import { applyDecorators } from '@nestjs/common';
import { ApiProperty } from '@nestjs/swagger';

export const CLASS_PROPERTIES: string = '__CLASS_PROPERTIES__';

export function IProperty<IsType extends IsValidType>(options?: PropertyOptions<IsType>): PropertyDecorator {
   const decorators: Array<ClassDecorator | MethodDecorator | PropertyDecorator> = [
      (target: Object, propertyKey: PropertyKey): void => {
         if (!target.hasOwnProperty(CLASS_PROPERTIES)) {
            target[CLASS_PROPERTIES] = {};
         }

         target[CLASS_PROPERTIES][propertyKey] = { ...options, swagger: undefined };
      },
   ];

   if (options?.swagger) {
      decorators.push(
         ApiProperty({
            description: options.swagger.description,
            example: options.swagger.example,
            required: options?.optional !== true,
         }),
      );
   }

   return applyDecorators(...decorators);
}
