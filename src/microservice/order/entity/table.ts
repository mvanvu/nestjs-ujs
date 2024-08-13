import { AvailableStatus, TableStatus } from '.prisma/order';
import { RestaurantRefEntity } from './restaurant';
import { UserRefEntity } from '@shared-library';
import { Schema } from '@mvanvu/ujs';

export class TableEntity {
   @Schema.mongoId().decorate()
   id: string;

   @Schema.classRef(RestaurantRefEntity).decorate()
   restaurant: RestaurantRefEntity;

   @Schema.enum(AvailableStatus).decorate()
   status: AvailableStatus;

   @Schema.enum(TableStatus).optional().decorate()
   activityStatus?: TableStatus;

   @Schema.uint(true).decorate()
   number: number;

   @Schema.content().optional().decorate()
   area?: string;

   @Schema.classRef(UserRefEntity).optional().decorate()
   author?: UserRefEntity;

   @Schema.classRef(UserRefEntity).optional().decorate()
   editor?: UserRefEntity;

   @Schema.dateTime().decorate()
   createdAt: Date;

   @Schema.dateTime().optional().decorate()
   updatedAt?: Date;
}
