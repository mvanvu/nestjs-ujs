import { Property } from '@lib/decorator';

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

   @Property({ optional: true, swagger: 'Advance filters' })
   filters?: string; // filters=id:1|2|3,email=admin@email.com
}
