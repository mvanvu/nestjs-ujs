import { AvailableStatus } from '.prisma/order';
import { ClassRefSchema, Schema } from '@mvanvu/ujs';

export class ItemToppingDto {
   @(Schema.content().decorate())
   name: string;

   @(Schema.uint().decorate())
   price: number;
}

export class CreateItemDto {
   @(Schema.mongoId().decorate())
   restaurantId: string;

   @(Schema.mongoId().decorate())
   categoryId: string;

   @(Schema.enum(AvailableStatus).optional().decorate())
   status?: AvailableStatus;

   @(Schema.content().decorate())
   name: string;

   @(Schema.imageUri().optional().decorate())
   imageUrl?: string;

   @(Schema.uint().decorate())
   basePrice: number;

   @(Schema.classRef(ItemToppingDto).array().optional().decorate())
   toppings?: ItemToppingDto[];
}

export class UpdateItemDto extends ClassRefSchema.Partial(CreateItemDto) {}
