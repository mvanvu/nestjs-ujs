import { applyDecorators } from '@nestjs/common';
import { Property } from './property';

type SchemaType = 'string' | 'number' | 'integer' | 'object' | 'array' | 'boolean' | 'null';

export function Schema(type: SchemaType | SchemaType[]) {
   return applyDecorators(Property({ schema: { type } }));
}

export function String() {
   return applyDecorators(Property({ schema: { type: 'string' } }));
}
