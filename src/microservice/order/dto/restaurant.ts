import { AvailableStatus } from '.prisma/content';
import { IPartialType, IsIn, IsMongoId, IsString, Property, UserRefEntity } from '@shared-library';

export class CreateRestaurantDto {
   @IsMongoId()
   ownerId: string;

   @Property({ swagger: { readOnly: true } })
   owner: UserRefEntity;

   @IsIn(Object.values(AvailableStatus), { optional: true })
   status?: AvailableStatus;

   @IsString({ notEmpty: true })
   name: string;

   @IsString({ optional: true, safeHtml: true })
   description?: string;

   @IsString({ optional: true, url: true })
   imageUrl?: string;

   @IsString({ optional: true })
   address?: string;

   @IsString({ optional: true })
   phoneNumber?: string;

   @IsString({ optional: true, email: true })
   email?: string;
}

export class UpdateRestaurantDto extends IPartialType(CreateRestaurantDto) {}
