import { AvailableStatus } from '.prisma/content';
import { Schema } from '@mvanvu/ujs';
import { IPickType, UserRefEntity } from '@shared-library';

export class RestaurantEntity {
   @Schema.mongoId().decorate()
   id: string;

   @Schema.classRef(UserRefEntity).decorate()
   owner: UserRefEntity;

   @Schema.enum(AvailableStatus).decorate()
   status: AvailableStatus;

   @Schema.content().decorate()
   name: string;

   @Schema.string().optional().decorate()
   description?: string;

   @Schema.content().optional().decorate()
   address?: string;

   @Schema.content().optional().decorate()
   phoneNumber?: string;

   @Schema.email().optional().decorate()
   email?: string;

   @Schema.classRef(UserRefEntity).optional().decorate()
   author?: UserRefEntity;

   @Schema.classRef(UserRefEntity).optional().decorate()
   editor?: UserRefEntity;

   @Schema.dateTime().decorate()
   createdAt: Date;

   @Schema.dateTime().optional().decorate()
   updatedAt?: Date;
}

export class RestaurantRefEntity extends IPickType(RestaurantEntity, ['id', 'name', 'owner']) {}
