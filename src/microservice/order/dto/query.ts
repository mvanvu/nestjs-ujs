import { PaginationQueryDto, Property } from '@shared-library';

export class OrderPaginationQueryDto extends PaginationQueryDto {
   @Property({ optional: true, validate: { is: 'mongoId' } })
   restaurantId?: string;

   @Property({ swagger: { disabled: true } })
   ownerId?: string;
}
