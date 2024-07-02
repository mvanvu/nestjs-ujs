import { AvailableStatus, TableStatus } from '.prisma/order';
import { RestaurantRefEntity } from './restaurant';
import { BaseEntity, EnumSchema, NumberSchema, ObjectSchema, StringSchema, UserRefEntity } from '@shared-library';

export class TableEntity extends BaseEntity {
   @StringSchema()
   id: string;

   @ObjectSchema(RestaurantRefEntity)
   restaurant: RestaurantRefEntity;

   @EnumSchema(Object.values(AvailableStatus))
   status: AvailableStatus;

   @EnumSchema(Object.values(TableStatus))
   activityStatus?: TableStatus;

   @NumberSchema({ integer: true, min: 1 })
   number: number;

   @StringSchema({ optional: true })
   area?: string;

   @ObjectSchema(UserRefEntity, { optional: true })
   author?: UserRefEntity;

   @ObjectSchema(UserRefEntity, { optional: true })
   editor?: UserRefEntity;

   @StringSchema({ format: 'date-time' })
   createdAt: Date;

   @StringSchema({ format: 'date-time' })
   updatedAt: Date;
}
