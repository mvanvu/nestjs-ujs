import { AvailableStatus } from '.prisma/content';
import { UserRefEntity } from '@shared-library';
import { MetadataEntity } from './metadata';
import { Schema } from '@mvanvu/ujs';

export class CategoryRef {
   @Schema.mongoId().decorate()
   id: string;

   @Schema.content().decorate()
   name: string;

   @Schema.content().decorate()
   path: string;
}

export class CategoryEntity {
   @Schema.mongoId().decorate()
   id: string;

   @Schema.enum(AvailableStatus).decorate()
   status: AvailableStatus;

   @Schema.content().decorate()
   name: string;

   @Schema.content().decorate()
   path: string;

   @Schema.string().optional().decorate()
   description?: string;

   @Schema.classRef(UserRefEntity).optional().decorate()
   author?: UserRefEntity;

   @Schema.classRef(UserRefEntity).optional().decorate()
   editor?: UserRefEntity;

   @Schema.dateTime().decorate()
   createdAt: Date;

   @Schema.dateTime().optional().decorate()
   updatedAt?: Date;

   @Schema.classRef(CategoryRef).optional().decorate()
   parent?: CategoryRef;

   @Schema.classRef(MetadataEntity).optional().decorate()
   metadata?: MetadataEntity;
}
