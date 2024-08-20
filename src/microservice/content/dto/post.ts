import { MetadataDto } from './metadata';
import { AvailableStatus } from '.prisma/content';
import { ClassRefSchema, Schema } from '@mvanvu/ujs';

export class CreatePostDto {
   @(Schema.enum(AvailableStatus).optional().decorate())
   status?: AvailableStatus;

   @(Schema.mongoId().optional().decorate())
   categoryId?: string;

   @(Schema.content().decorate())
   title: string;

   @(Schema.content().format('slug').optional().decorate())
   slug?: string;

   @(Schema.safeHTML().optional().decorate())
   description?: string;

   @(Schema.imageUri().optional().decorate())
   imageUrl?: string;

   @(Schema.classRef(MetadataDto).optional().decorate())
   metadata?: MetadataDto;

   @(Schema.mongoId().array().unique().optional().decorate())
   tags?: string[];
}

export class UpdatePostDto extends ClassRefSchema.Partial(CreatePostDto) {}
