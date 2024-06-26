import { i18n } from '../i18n';
import { Property } from '../decorator/property';

export class PaginationQueryDto {
   @Property({
      transform: { fromType: 'string', toType: 'trim' },
      optional: true,
      swagger: { description: 'The search query' },
   })
   q?: string;

   @Property({
      transform: { fromType: 'string', toType: 'toNumber' },
      validate: { is: 'uInt' },
      optional: true,
      swagger: { description: 'The page of the pagination' },
   })
   page?: number;

   @Property({
      transform: { fromType: 'string', toType: 'toNumber' },
      validate: { is: 'uInt' },
      optional: true,
      swagger: { description: 'The number of items on one row' },
   })
   limit?: number;

   @Property({
      transform: { fromType: 'string', toType: 'trim' },
      optional: true,
      swagger: { description: 'Order by' },
   })
   order?: string;

   @Property({
      transform: { fromType: 'string', toType: 'trim' },
      optional: true,
      swagger: {
         description: 'Language code',
         enum: Object.keys(i18n).map((code) => `${code.substring(0, 2)}-${code.substring(2)}`),
      },
   })
   lang?: string;
}
