import { i18n } from '../i18n';
import { IsIn, IsString, StringToType } from '../decorator/validator';

const langCodes = Object.keys(i18n).map((code) => `${code.substring(0, 2)}-${code.substring(2)}`);

export class PaginationQueryDto {
   @IsString({ optional: true, swagger: { description: 'The search query' } })
   q?: string;

   @StringToType('uInt', { optional: true, swagger: { description: 'The current page' } })
   page?: number;

   @StringToType('uInt', { optional: true, swagger: { description: 'The number of items on one row' } })
   limit?: number;

   @IsString({ optional: true, swagger: { description: 'Order by' } })
   order?: string;

   @IsIn(langCodes, {
      optional: true,
      swagger: { description: 'Language code', enum: langCodes },
   })
   lang?: string;
}
