import { EnumSchema, IPartialType, ObjectSchema, StringSchema } from '@shared-library';
import { MetadataDto } from './metadata';
import { AvailableStatus } from '.prisma/content';

export class CreateCategoryDto {
   @StringSchema({ optional: true, format: 'mongoId' })
   parentId?: string;

   @EnumSchema(Object.values(AvailableStatus), { optional: true })
   status?: AvailableStatus;

   @StringSchema({ notEmpty: true })
   name: string;

   @StringSchema({ optional: true })
   description?: string;

   @ObjectSchema(MetadataDto, { optional: true })
   metadata?: MetadataDto;
}

export class UpdateCategoryDto extends IPartialType(CreateCategoryDto) {}
