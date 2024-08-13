import { StaffStatus } from '.prisma/order';
import { Schema } from '@mvanvu/ujs';
import { UserRefEntity } from '@shared-library';

export class StaffEntity {
   @Schema.mongoId().decorate()
   id: string;

   @Schema.enum(StaffStatus).decorate()
   status: StaffStatus;

   @Schema.content().decorate()
   name: string;

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
