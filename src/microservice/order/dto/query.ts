import { IsMongoId, PaginationQueryDto, Property } from '@shared-library';

export class OrderPaginationQueryDto extends PaginationQueryDto {
   @IsMongoId({ optional: true })
   restaurantId?: string;

   @Property({ swagger: { disabled: true } })
   ownerId?: string;
}
