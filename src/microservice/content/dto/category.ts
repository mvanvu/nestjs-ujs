import { MetadataDto } from './metadata';
import { AvailableStatus } from '.prisma/content';
import { ClassRefSchema, Schema } from '@mvanvu/ujs';

export class CreateCategoryDto {
   @(Schema.mongoId().optional().decorate())
   parentId?: string;

   @(Schema.enum(AvailableStatus).optional().decorate())
   status?: AvailableStatus;

   @(Schema.content().decorate())
   title: string;

   @(Schema.content().format('slug').optional().decorate())
   slug?: string;

   @(Schema.string().optional().decorate())
   description?: string;

   @(Schema.classRef(MetadataDto).optional().decorate())
   metadata?: MetadataDto;
}

export class UpdateCategoryDto extends ClassRefSchema.Partial(CreateCategoryDto) {}
