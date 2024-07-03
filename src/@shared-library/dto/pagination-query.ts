import { i18n } from '../i18n';
import { EnumSchema, StringSchema } from '../decorator/schema';

const langCodes = Object.keys(i18n).map((code) => `${code.substring(0, 2)}-${code.substring(2)}`);

export class PaginationQueryDto {
   @StringSchema({ optional: true, empty: 'skip' })
   q?: string;

   @StringSchema({ optional: true, format: 'unsignedInteger', empty: 'skip' })
   page?: number;

   @StringSchema({ optional: true, format: 'unsignedInteger', empty: 'skip' })
   limit?: number;

   @StringSchema({ optional: true })
   order?: string;

   @EnumSchema(langCodes, { optional: true })
   lang?: string;
}
