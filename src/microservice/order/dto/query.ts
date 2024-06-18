import { Property } from '@lib/decorator/property';
import { PaginationQueryDto } from '@lib/type/dto';

export class OrderPaginationQueryDto extends PaginationQueryDto {
   @Property({ optional: true, validate: { is: 'mongoId' } })
   restaurantId?: string;

   @Property({ swagger: { disable: true } })
   ownerId?: string;
}
