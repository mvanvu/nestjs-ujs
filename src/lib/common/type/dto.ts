import { EntityProperty } from '@lib/common/decorator';
import { ApiProperty } from '@nestjs/swagger';

export class PaginationQueryDto {
   @ApiProperty({ description: 'The search query' })
   @EntityProperty({ transform: { fromType: 'string', toType: 'trim' }, optional: true })
   q?: string;

   @ApiProperty({ description: 'The page of the pagination' })
   @EntityProperty({ transform: { fromType: 'string', toType: 'toNumber' }, validate: { is: 'uInt' }, optional: true })
   page?: number;

   @ApiProperty({ description: 'The number of items on one row' })
   @EntityProperty({ transform: { fromType: 'string', toType: 'toNumber' }, validate: { is: 'uInt' }, optional: true })
   limit?: number;

   @ApiProperty({ description: 'Order by' })
   @EntityProperty({ transform: { fromType: 'string', toType: 'trim' }, optional: true })
   order?: string;

   @ApiProperty({ description: 'Language code' })
   @EntityProperty({ transform: { fromType: 'string', toType: 'trim' }, optional: true })
   lang?: string;
}
