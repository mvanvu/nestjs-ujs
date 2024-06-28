import { IPartialType, IsIn, IsMongoId, IsString, IsDTO } from '@shared-library';
import { MetadataDto } from './metadata';
import { AvailableStatus } from '.prisma/content';

export class CreatePostDto {
   @IsIn(Object.values(AvailableStatus), { optional: true })
   status?: AvailableStatus;

   @IsMongoId({ optional: true })
   categoryId?: string;

   @IsString({ notEmpty: true })
   name: string;

   @IsString({ optional: true })
   description?: string;

   @IsString({ optional: true, url: true })
   imageUrl?: string;

   @IsDTO(MetadataDto, { optional: true })
   metadata?: MetadataDto;
}

export class UpdatePostDto extends IPartialType(CreatePostDto) {}
