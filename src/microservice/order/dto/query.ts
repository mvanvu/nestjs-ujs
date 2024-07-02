import { PaginationQueryDto, StringSchema } from '@shared-library';

export class OrderPaginationQueryDto extends PaginationQueryDto {
   @StringSchema({ optional: true, format: 'mongoId' })
   restaurantId?: string;

   @StringSchema({ format: 'mongoId', swagger: false })
   ownerId?: string;
}
