import { AvailableStatus, TableStatus } from '.prisma/order';
import { RestaurantRefEntity } from './restaurant';
import { BaseEntity, EnumSchema, NumberSchema, ClassSchema, StringSchema, UserRefEntity } from '@shared-library';

export class TableEntity extends BaseEntity {
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

   @StringSchema({ format: 'date-time' })
   createdAt: Date;

   @StringSchema({ format: 'date-time' })
   updatedAt: Date;
}
