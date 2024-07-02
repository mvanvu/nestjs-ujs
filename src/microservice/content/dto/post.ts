import { EnumSchema, IPartialType, ObjectSchema, StringSchema } from '@shared-library';
import { MetadataDto } from './metadata';
import { AvailableStatus } from '.prisma/content';

export class CreatePostDto {
   @EnumSchema(Object.values(AvailableStatus), { optional: true })
   status?: AvailableStatus;

   @StringSchema({ optional: true })
   categoryId?: string;

   @StringSchema({ notEmpty: true })
   name: string;

   @StringSchema({ optional: true })
   description?: string;

   @StringSchema({ optional: true, format: 'url' })
   imageUrl?: string;

   @ObjectSchema(MetadataDto, { optional: true })
   metadata?: MetadataDto;
}

export class UpdatePostDto extends IPartialType(CreatePostDto) {}
