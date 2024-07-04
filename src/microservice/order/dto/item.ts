import { AvailableStatus } from '.prisma/order';
import {
   EnumSchema,
   IPartialType,
   NumberSchema,
   ClassSchema,
   IDSchema,
   NameSchema,
   ImageSchema,
   UIntSchema,
} from '@shared-library';

export class ItemToppingDto {
   @NameSchema()
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

   @NameSchema()
   name: string;

   @ImageSchema({ optional: true })
   imageUrl?: string;

   @UIntSchema()
   basePrice: number;

   @ClassSchema(ItemToppingDto, { optional: true, isArray: 'unique' })
   toppings?: ItemToppingDto[];
}

export class UpdateItemDto extends IPartialType(CreateItemDto) {}
