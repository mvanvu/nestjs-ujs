import { IDSchema, PaginationQueryDto } from '@shared-library';

export class OrderPaginationQueryDto extends PaginationQueryDto {
   @IDSchema({ optional: true })
   restaurantId?: string;

   @IDSchema({ optional: true, swagger: false })
   ownerId?: string;
}
