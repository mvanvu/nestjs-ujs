import { metadata } from '@lib/metadata';
import { PropertyOptions } from '@lib/common/type';
import { IsValidType, Registry } from '@mvanvu/ujs';
import { applyDecorators } from '@nestjs/common';
import { ApiProperty } from '@nestjs/swagger';

export const CLASS_PROPERTIES: string = '__CLASS_PROPERTIES__';

export function Property<IsType extends IsValidType>(options?: PropertyOptions<IsType>): PropertyDecorator {
   const decorators: Array<ClassDecorator | MethodDecorator | PropertyDecorator> = [
      (target: Object, propertyKey: PropertyKey): void => {
         if (!target.hasOwnProperty(CLASS_PROPERTIES)) {
            target[CLASS_PROPERTIES] = {};
         }

         target[CLASS_PROPERTIES][propertyKey] = Registry.from(options).omit('swagger').valueOf();
      },
   ];

   if (metadata.isGateway()) {
      if (options?.swagger) {
         if (typeof options?.swagger === 'string') {
            decorators.push(ApiProperty({ description: options.swagger, required: options?.optional !== true }));
         } else {
            decorators.push(
               ApiProperty({
                  description: options.swagger.description,
                  example: options.swagger.example,
                  required: options?.optional !== true,
                  isArray: options.swagger.isArray,
                  type: options.swagger.type,
                  enum: options.swagger.enum,
               }),
            );
         }
      } else {
         decorators.push(ApiProperty());
      }
   }

   return applyDecorators(...decorators);
}
