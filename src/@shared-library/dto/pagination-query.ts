import { Schema } from '@mvanvu/ujs';
import { i18n } from '../i18n';

const langCodes = Object.keys(i18n).map((code) => `${code.substring(0, 2)}-${code.substring(2)}`);

export class PaginationQueryDto {
   @Schema.string().optional().decorate()
   q?: string;

   @Schema.strUInt().optional().decorate()
   page?: number;

   @Schema.strUInt().optional().decorate()
   limit?: number;

   @Schema.trim().optional().decorate()
   order?: string;

   @Schema.enum(langCodes).optional().decorate()
   lang?: string;
}
