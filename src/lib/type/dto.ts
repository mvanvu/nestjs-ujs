import { IProperty } from '@lib/decorator';

export class PaginationQueryDto {
   @IProperty({ transform: { fromType: 'string', toType: 'trim' }, optional: true, swagger: 'The search query' })
   q?: string;

   @IProperty({
      transform: { fromType: 'string', toType: 'toNumber' },
      validate: { is: 'uInt' },
      optional: true,
      swagger: 'The page of the pagination',
   })
   page?: number;

   @IProperty({
      transform: { fromType: 'string', toType: 'toNumber' },
      validate: { is: 'uInt' },
      optional: true,
      swagger: 'The number of items on one row',
   })
   limit?: number;

   @IProperty({
      transform: { fromType: 'string', toType: 'trim' },
      optional: true,
      swagger: 'Order by',
   })
   order?: string;

   @IProperty({
      transform: { fromType: 'string', toType: 'trim' },
      optional: true,
      swagger: 'Language code',
   })
   lang?: string;

   @IProperty({ optional: true, swagger: 'Advance filters' })
   filters?: string; // filters=id:1|2|3,email=admin@email.com
}
