import { AvailableStatus } from '.prisma/order';
import { EnumSchema, IPartialType, NumberSchema, ObjectSchema, StringSchema } from '@shared-library';

export class ItemToppingDto {
   @StringSchema({ notEmpty: true })
   name: string;

   @NumberSchema({ min: 0, integer: true })
   price: number;
}

export class CreateItemDto {
   @StringSchema({ format: 'mongoId' })
   restaurantId: string;

   @StringSchema({ format: 'mongoId' })
   categoryId: string;

   @EnumSchema(Object.values(AvailableStatus))
   status?: AvailableStatus;

   @StringSchema({ notEmpty: true })
   name: string;

   @StringSchema({ optional: true, format: 'url' })
   imageUrl?: string;

   @NumberSchema({ min: 0, integer: true })
   basePrice: number;

   @ObjectSchema(ItemToppingDto, { optional: true, each: 'unique' })
   toppings?: ItemToppingDto[];
}

export class UpdateItemDto extends IPartialType(CreateItemDto) {}
