import { AvailableStatus } from '.prisma/content';
import { CategoryRef } from './category';
import { BaseEntity, EnumSchema, NumberSchema, ObjectSchema, StringSchema, UserRefEntity } from '@shared-library';

export class ToppingEntity extends BaseEntity {
   @StringSchema()
   name: string;

   @NumberSchema({ min: 0, integer: true })
   price: number;
}

export class ItemEntity extends BaseEntity {
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

   @ObjectSchema(UserRefEntity)
   author?: UserRefEntity;

   @ObjectSchema(UserRefEntity)
   editor?: UserRefEntity;

   @StringSchema({ format: 'date-time' })
   createdAt: Date;

   @StringSchema({ format: 'date-time' })
   updatedAt?: Date;

   @ObjectSchema(CategoryRef, { optional: true })
   category?: CategoryRef;

   @ObjectSchema(ToppingEntity, { optional: true })
   topping?: ToppingEntity;
}
