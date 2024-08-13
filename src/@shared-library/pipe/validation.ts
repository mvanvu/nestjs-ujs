import { ArgumentMetadata, HttpStatus, Injectable, PipeTransform } from '@nestjs/common';
import { BaseSchema, ClassRefSchema, Is } from '@mvanvu/ujs';
import { ClassConstructor } from '../type/common';
import { ThrowException } from '../exception/throw';

export function validateDTO(data: any, dtoClassRef: ClassConstructor<any>): any {
   if (!Is.class(dtoClassRef)) {
      ThrowException(`${dtoClassRef} must be a valid DTO class constructor`, HttpStatus.NOT_IMPLEMENTED);
   }

   const schema = dtoClassRef instanceof BaseSchema ? dtoClassRef : new ClassRefSchema(dtoClassRef);

   if (!schema.check(data)) {
      ThrowException(schema.getErrors());
   }

   return schema.getValue();
}

@Injectable()
export class ValidationPipe implements PipeTransform {
   transform(value: any, meta: ArgumentMetadata): any {
      const { metatype: ClassContructor } = meta;

      return Is.class(ClassContructor) ? validateDTO(value, ClassContructor) : value;
   }
}
