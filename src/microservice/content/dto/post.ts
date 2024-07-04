import {
   EnumSchema,
   IPartialType,
   ClassSchema,
   StringSchema,
   IDSchema,
   NameSchema,
   ImageSchema,
} from '@shared-library';
import { MetadataDto } from './metadata';
import { AvailableStatus } from '.prisma/content';

export class CreatePostDto {
   @EnumSchema(Object.values(AvailableStatus), { optional: true })
   status?: AvailableStatus;

   @IDSchema({ optional: true })
   categoryId?: string;

   @NameSchema()
   name: string;

   @StringSchema({ optional: true })
   description?: string;

   @ImageSchema({ optional: true })
   imageUrl?: string;

   @ClassSchema(MetadataDto, { optional: true })
   metadata?: MetadataDto;
}

export class UpdatePostDto extends IPartialType(CreatePostDto) {}
