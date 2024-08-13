import { AvailableStatus } from '.prisma/content';
import { CategoryRef } from './category';
import { UserRefEntity } from '@shared-library';
import { Schema } from '@mvanvu/ujs';

export class ToppingEntity {
   @Schema.content().decorate()
   name: string;

   @Schema.uint().decorate()
   price: number;
}

export class ItemEntity {
   @Schema.mongoId().decorate()
   id: string;

   @Schema.enum(AvailableStatus).decorate()
   status: AvailableStatus;

   @Schema.content().decorate()
   name: string;

   @Schema.imageUri().optional().decorate()
   imageUrl?: string;

   @Schema.uint().decorate()
   basePrice: number;

   @Schema.classRef(UserRefEntity).decorate()
   author?: UserRefEntity;

   @Schema.classRef(UserRefEntity).decorate()
   editor?: UserRefEntity;

   @Schema.dateTime().decorate()
   createdAt: Date;

   @Schema.dateTime().optional().decorate()
   updatedAt?: Date;

   @Schema.classRef(CategoryRef).optional().decorate()
   category?: CategoryRef;

   @Schema.classRef(ToppingEntity).optional().decorate()
   topping?: ToppingEntity;
}
