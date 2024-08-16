import { Schema } from '@mvanvu/ujs';
import { PaginationQueryDto } from '@shared-library';

export class OrderPaginationQueryDto extends PaginationQueryDto {
   @(Schema.mongoId().optional().decorate())
   restaurantId?: string;

   @(Schema.mongoId().optional().decorate())
   ownerId?: string;
}
