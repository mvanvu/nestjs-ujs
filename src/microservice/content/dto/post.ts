import { IPartialType } from '@shared-library';
import { MetadataDto } from './metadata';
import { AvailableStatus } from '.prisma/content';
import { Schema } from '@mvanvu/ujs';

export class CreatePostDto {
   @Schema.enum(AvailableStatus).optional().decorate()
   status?: AvailableStatus;

   @Schema.mongoId().optional().decorate()
   categoryId?: string;

   @Schema.content().decorate()
   name: string;

   @Schema.safeHTML().optional().decorate()
   description?: string;

   @Schema.imageUri().optional().decorate()
   imageUrl?: string;

   @Schema.classRef(MetadataDto).optional().decorate()
   metadata?: MetadataDto;
}

export class UpdatePostDto extends IPartialType(CreatePostDto) {}
