import { AvailableStatus } from '.prisma/content';
import { UserRefEntity } from '@shared-library';
import { CategoryRef } from './category';
import { MetadataEntity } from './metadata';
import { Schema } from '@mvanvu/ujs';
import { TagRef } from './tag';

export class PostEntity {
   @(Schema.mongoId().decorate())
   id: string;

   @(Schema.enum(AvailableStatus).decorate())
   status: AvailableStatus;

   @(Schema.content().decorate())
   title: string;

   @(Schema.content().format('slug').decorate())
   slug: string;

   @(Schema.content().format('path').decorate())
   path: string;

   @(Schema.string().optional().decorate())
   description?: string;

   @(Schema.imageUri().optional().decorate())
   imageUrl?: string;

   @(Schema.classRef(UserRefEntity).optional().decorate())
   author?: UserRefEntity;

   @(Schema.classRef(UserRefEntity).optional().decorate())
   editor?: UserRefEntity;

   @(Schema.dateTime().decorate())
   createdAt: Date;

   @(Schema.dateTime().optional().decorate())
   updatedAt?: Date;

   @(Schema.classRef(CategoryRef).optional().decorate())
   category?: CategoryRef;

   @(Schema.classRef(MetadataEntity).optional().decorate())
   metadata?: MetadataEntity;

   @(Schema.classRef(TagRef).array().decorate())
   tags: TagRef[];
}
