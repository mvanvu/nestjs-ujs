import { StaffStatus } from '.prisma/order';
import { Schema } from '@mvanvu/ujs';
import { IPartialType } from '@shared-library';

export class CreateStaffDto {
   @Schema.mongoId().decorate()
   restaurantId: string;

   @Schema.enum(StaffStatus).optional().decorate()
   status?: StaffStatus;

   @Schema.content().decorate()
   name: string;

   @Schema.imageUri().optional().decorate()
   imageUrl?: string;

   @Schema.content().optional().decorate()
   phoneNumber?: string;

   @Schema.email().optional().decorate()
   email?: string;
}

export class UpdateStaffDto extends IPartialType(CreateStaffDto) {}
