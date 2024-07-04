import { AvailableStatus } from '.prisma/content';
import { CategoryRef } from './category';
import {
   EnumSchema,
   ClassSchema,
   StringSchema,
   UserRefEntity,
   DateSchema,
   IDSchema,
   ImageSchema,
   UIntSchema,
} from '@shared-library';

export class ToppingEntity {
   @StringSchema()
   name: string;

   @UIntSchema()
   price: number;
}

export class ItemEntity {
   @IDSchema()
   id: string;

   @EnumSchema(Object.values(AvailableStatus))
   status: AvailableStatus;

   @StringSchema()
   name: string;

   @ImageSchema({ optional: true })
   imageUrl?: string;

   @UIntSchema()
   basePrice: number;

   @ClassSchema(UserRefEntity, { optional: true })
   author?: UserRefEntity;

   @ClassSchema(UserRefEntity, { optional: true })
   editor?: UserRefEntity;

   @DateSchema()
   createdAt: Date;

   @DateSchema()
   updatedAt: Date;

   @ClassSchema(CategoryRef, { optional: true })
   category?: CategoryRef;

   @ClassSchema(ToppingEntity, { optional: true })
   topping?: ToppingEntity;
}
