import { AvailableStatus } from '.prisma/content';
import { CategoryRef } from './category';
import { EnumSchema, NumberSchema, ClassSchema, StringSchema, UserRefEntity, DateSchema } from '@shared-library';

export class ToppingEntity {
   @StringSchema()
   name: string;

   @NumberSchema({ min: 0, integer: true })
   price: number;
}

export class ItemEntity {
   @StringSchema()
   id: string;

   @EnumSchema(Object.values(AvailableStatus))
   status: AvailableStatus;

   @StringSchema()
   name: string;

   @StringSchema({ format: 'url' })
   imageUrl?: string;

   @NumberSchema({ min: 0, integer: true })
   basePrice: number;

   @ClassSchema(UserRefEntity)
   author?: UserRefEntity;

   @ClassSchema(UserRefEntity)
   editor?: UserRefEntity;

   @DateSchema()
   createdAt: Date;

   @DateSchema()
   updatedAt?: Date;

   @ClassSchema(CategoryRef, { optional: true })
   category?: CategoryRef;

   @ClassSchema(ToppingEntity, { optional: true })
   topping?: ToppingEntity;
}
