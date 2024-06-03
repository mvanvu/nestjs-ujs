import { IProperty } from '@lib/common/decorator';

export class PaginationQueryDto {
   @IProperty({
      transform: { fromType: 'string', toType: 'trim' },
      optional: true,
      swagger: { description: 'The search query' },
   })
   q?: string;

   @IProperty({
      transform: { fromType: 'string', toType: 'toNumber' },
      validate: { is: 'uInt' },
      optional: true,
      swagger: { description: 'The page of the pagination' },
   })
   page?: number;

   @IProperty({
      transform: { fromType: 'string', toType: 'toNumber' },
      validate: { is: 'uInt' },
      optional: true,
      swagger: { description: 'The number of items on one row' },
   })
   limit?: number;

   @IProperty({
      transform: { fromType: 'string', toType: 'trim' },
      optional: true,
      swagger: { description: 'Order by' },
   })
   order?: string;

   @IProperty({
      transform: { fromType: 'string', toType: 'trim' },
      optional: true,
      swagger: { description: 'Language code' },
   })
   lang?: string;
}
