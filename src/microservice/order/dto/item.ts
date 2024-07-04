import { AvailableStatus } from '.prisma/order';
import { EnumSchema, IPartialType, NumberSchema, ClassSchema, StringSchema, IDSchema } from '@shared-library';

export class ItemToppingDto {
   @StringSchema({ empty: false })
   name: string;

   @NumberSchema({ min: 0, integer: true })
   price: number;
}

export class CreateItemDto {
   @IDSchema()
   restaurantId: string;

   @IDSchema()
   categoryId: string;

   @EnumSchema(Object.values(AvailableStatus))
   status?: AvailableStatus;

   @StringSchema({ empty: false })
   name: string;

   @StringSchema({ optional: true, format: 'url' })
   imageUrl?: string;

   @NumberSchema({ min: 0, integer: true })
   basePrice: number;

   @ClassSchema(ItemToppingDto, { optional: true, isArray: 'unique' })
   toppings?: ItemToppingDto[];
}

export class UpdateItemDto extends IPartialType(CreateItemDto) {}
