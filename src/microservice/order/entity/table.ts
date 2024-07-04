import { AvailableStatus, TableStatus } from '.prisma/order';
import { RestaurantRefEntity } from './restaurant';
import { EnumSchema, NumberSchema, ClassSchema, StringSchema, UserRefEntity, DateSchema } from '@shared-library';

export class TableEntity {
   @StringSchema()
   id: string;

   @ClassSchema(RestaurantRefEntity)
   restaurant: RestaurantRefEntity;

   @EnumSchema(Object.values(AvailableStatus))
   status: AvailableStatus;

   @EnumSchema(Object.values(TableStatus))
   activityStatus?: TableStatus;

   @NumberSchema({ integer: true, min: 1 })
   number: number;

   @StringSchema({ optional: true })
   area?: string;

   @ClassSchema(UserRefEntity, { optional: true })
   author?: UserRefEntity;

   @ClassSchema(UserRefEntity, { optional: true })
   editor?: UserRefEntity;

   @DateSchema()
   createdAt: Date;

   @DateSchema()
   updatedAt: Date;
}
