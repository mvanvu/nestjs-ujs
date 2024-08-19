import { BaseSchema, Schema } from '@mvanvu/ujs';
import { ThrowException } from '@shared-library';
import { PipeTransform } from '@nestjs/common';

export function ParseTypePipe(
   keyCheck: Exclude<keyof typeof Schema, 'classRef' | 'prototype' | 'enum' | 'password' | 'regex'>,
) {
   return new (class ParseSchemaPipe implements PipeTransform<any, string> {
      transform(value: any): string {
         const schema = (Schema[keyCheck] as () => BaseSchema)();

         if (schema instanceof BaseSchema && !schema.check(value)) {
            ThrowException(schema.getErrors());
         }

         return schema.getValue();
      }
   })();
}

export function ParseEnumPipe(enumElements: any[]) {
   return new (class ParseSchemaPipe implements PipeTransform<any, string> {
      transform(value: any): any {
         const schema = Schema.enum(enumElements);

         if (!schema.check(value)) {
            ThrowException(schema.getErrors());
         }

         return schema.getValue();
      }
   })();
}

export function ParseSchemaPipe(schema: BaseSchema) {
   return new (class ParseSchemaPipe implements PipeTransform<any, string> {
      transform(value: any): any {
         if (!schema.check(value)) {
            ThrowException(schema.getErrors());
         }

         return schema.getValue();
      }
   })();
}

export class ParseMongoIdPipe implements PipeTransform<any, string> {
   transform(value: any): string {
      const schema = Schema.mongoId();

      if (!schema.check(value)) {
         ThrowException(schema.getErrors());
      }

      return schema.getValue();
   }
}

export class ParseIntIdPipe implements PipeTransform<any, string> {
   transform(value: any): string {
      const schema = Schema.uint(true);

      if (!schema.check(value)) {
         ThrowException(schema.getErrors());
      }

      return schema.getValue();
   }
}
