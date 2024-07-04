import { AvailableStatus, TableStatus } from '.prisma/order';
import { RestaurantRefEntity } from './restaurant';
import {
   EnumSchema,
   ClassSchema,
   StringSchema,
   UserRefEntity,
   DateSchema,
   IDSchema,
   UIntSchema,
} from '@shared-library';

export class TableEntity {
   @IDSchema()
   id: string;

   @ClassSchema(RestaurantRefEntity)
   restaurant: RestaurantRefEntity;

   @EnumSchema(Object.values(AvailableStatus))
   status: AvailableStatus;

   @EnumSchema(Object.values(TableStatus), { optional: true })
   activityStatus?: TableStatus;

   @UIntSchema({ min: 1 })
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
