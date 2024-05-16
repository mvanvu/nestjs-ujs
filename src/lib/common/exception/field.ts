import { ThrowException } from './throw';
import { Is } from '@mvanvu/ujs';

export type FieldsError = Record<string, Array<string | number>>;

export class FieldsException {
   static readonly ALREADY_EXISTS = 'ALREADY_EXISTS';
   static readonly NOT_FOULND = 'NOT_FOULND';
   static readonly REQUIRED = 'REQUIRED';
   static readonly UNIQUE_CONSTRAINT = 'UNIQUE_CONSTRAINT';

   private fieldsError: FieldsError = {};

   add(name: string, code: string | number): this {
      if (!Array.isArray(this.fieldsError[name])) {
         this.fieldsError[name] = [];
      }

      if (!this.fieldsError[name].includes(code)) {
         this.fieldsError[name].push(code);
      }

      return this;
   }

   validate(): void {
      if (!Is.emptyObject(this.fieldsError)) {
         new ThrowException(this.fieldsError);
      }
   }
}
