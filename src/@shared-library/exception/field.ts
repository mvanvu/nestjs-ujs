import { ThrowException } from './throw';
import { Is } from '@mvanvu/ujs';

export type FieldsError = Record<string, Array<{ code: string | number; message?: string }>>;

export class FieldsException {
   static readonly ALREADY_EXISTS = 'ALREADY_EXISTS';
   static readonly NOT_FOULND = 'NOT_FOULND';
   static readonly REQUIRED = 'REQUIRED';
   static readonly UNIQUE_CONSTRAINT = 'UNIQUE_CONSTRAINT';

   private fieldsError: FieldsError = {};

   add(name: string, code: string | number, message?: string): this {
      if (!Array.isArray(this.fieldsError[name])) {
         this.fieldsError[name] = [];
      }

      if (!this.fieldsError[name].find((e) => e.code === code)) {
         this.fieldsError[name].push({ code, message });
      }

      return this;
   }

   validate(): void {
      if (!Is.empty(this.fieldsError)) {
         ThrowException(this.fieldsError);
      }
   }
}
