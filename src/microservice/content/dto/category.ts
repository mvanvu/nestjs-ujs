import { EnumSchema, IPartialType, ClassSchema, StringSchema, IDSchema, NameSchema } from '@shared-library';
import { MetadataDto } from './metadata';
import { AvailableStatus } from '.prisma/content';

export class CreateCategoryDto {
   @IDSchema({ optional: true })
   parentId?: string;

   @EnumSchema(Object.values(AvailableStatus), { optional: true })
   status?: AvailableStatus;

   @NameSchema()
   name: string;

   @StringSchema({ optional: true })
   description?: string;

   @ClassSchema(MetadataDto, { optional: true })
   metadata?: MetadataDto;
}

export class UpdateCategoryDto extends IPartialType(CreateCategoryDto) {}
