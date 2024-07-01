import { applyDecorators } from '@nestjs/common';
import { Property } from './property';
import { ClassConstructor } from '@mvanvu/ujs';

type SchemaType = 'string' | 'number' | 'integer' | 'boolean';

export function Schema(type: SchemaType | ClassConstructor<any> | [ClassConstructor<any>], nullable?: boolean) {
   return applyDecorators(Property({ schema: { type } }));
}
