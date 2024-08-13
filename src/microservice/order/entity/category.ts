import { AvailableStatus } from '.prisma/order';
import { Schema } from '@mvanvu/ujs';
import { UserRefEntity } from '@shared-library';

export class CategoryEntity {
   @Schema.mongoId().decorate()
   id: string;

   @Schema.enum(AvailableStatus).decorate()
   status: AvailableStatus;

   @Schema.content().decorate()
   name: string;

   @Schema.classRef(UserRefEntity).optional().decorate()
   author?: UserRefEntity;

   @Schema.classRef(UserRefEntity).optional().decorate()
   editor?: UserRefEntity;

   @Schema.dateTime().decorate()
   createdAt: Date;

   @Schema.dateTime().optional().decorate()
   updatedAt?: Date;
}

export class CategoryRef {
   @Schema.mongoId().decorate()
   id: string;

   @Schema.content().decorate()
   name: string;
}
