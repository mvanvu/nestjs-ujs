import { Property } from '@lib/common/decorator/property';
import { PaginationQueryDto } from '@lib/common/type/dto';

export class OrderPaginationQueryDto extends PaginationQueryDto {
   @Property({ optional: true, validate: { is: 'mongoId' } })
   restaurantId?: string;

   @Property({ swagger: { disabled: true } })
   ownerId?: string;
}
