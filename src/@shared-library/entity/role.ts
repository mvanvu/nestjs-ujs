import { $Enums } from '.prisma/user';
import { Schema } from '@mvanvu/ujs';

export class RoleEntity {
   @Schema.mongoId().decorate()
   id: string;

   @Schema.enum(Object.values($Enums.AvailableStatus)).decorate()
   status: $Enums.AvailableStatus;

   @Schema.content().decorate()
   name: string;

   @Schema.string().optional().decorate()
   description?: string;

   @Schema.dateTime().decorate()
   createdAt: Date;

   @Schema.dateTime().optional().decorate()
   updatedAt?: Date;

   @Schema.string().array().decorate()
   permissions: string[];
}
