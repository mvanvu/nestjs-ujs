import { Property } from '@lib/common/decorator';

export class PaginationQueryDto {
   @Property({ transform: { fromType: 'string', toType: 'trim' }, optional: true, swagger: 'The search query' })
   q?: string;

   @Property({
      transform: { fromType: 'string', toType: 'toNumber' },
      validate: { is: 'uInt' },
      optional: true,
      swagger: 'The page of the pagination',
   })
   page?: number;

   @Property({
      transform: { fromType: 'string', toType: 'toNumber' },
      validate: { is: 'uInt' },
      optional: true,
      swagger: 'The number of items on one row',
   })
   limit?: number;

   @Property({
      transform: { fromType: 'string', toType: 'trim' },
      optional: true,
      swagger: 'Order by',
   })
   order?: string;

   @Property({
      transform: { fromType: 'string', toType: 'trim' },
      optional: true,
      swagger: 'Language code',
   })
   lang?: string;
}
