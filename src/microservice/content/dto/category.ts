import { IPartialType, IsMongoId, IsIn, IsString, IsDTO } from '@shared-library';
import { MetadataDto } from './metadata';
import { AvailableStatus } from '.prisma/content';

export class CreateCategoryDto {
   @IsMongoId({ optional: true })
   parentId?: string;

   @IsIn(Object.values(AvailableStatus), { optional: true })
   status?: AvailableStatus;

   @IsString({ notEmpty: true })
   name: string;

   @IsString({ optional: true })
   description?: string;

   @IsDTO(MetadataDto, { optional: true })
   metadata?: MetadataDto;
}

export class UpdateCategoryDto extends IPartialType(CreateCategoryDto) {}
