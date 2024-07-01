import { i18n } from '../i18n';
import { EnumSchema, NumberSchema, StringSchema } from '../decorator/schema';

const langCodes = Object.keys(i18n).map((code) => `${code.substring(0, 2)}-${code.substring(2)}`);

export class PaginationQueryDto {
   @StringSchema({ optional: true })
   q?: string;

   @NumberSchema({ optional: true, fromString: true, integer: true, min: 1 })
   page?: number;

   @NumberSchema({ optional: true, fromString: true, integer: true, min: 1 })
   limit?: number;

   @StringSchema({ optional: true })
   order?: string;

   @EnumSchema(langCodes, { optional: true })
   lang?: string;
}
