import { StaffStatus } from '.prisma/order';
import { IPartialType, IsIn, IsMongoId, IsString } from '@shared-library';

export class CreateStaffDto {
   @IsMongoId()
   restaurantId: string;

   @IsIn(Object.values(StaffStatus), { optional: true })
   status?: StaffStatus;

   @IsString({ optional: true, notEmpty: true })
   name: string;

   @IsString({ optional: true, url: true })
   imageUrl?: string;

   @IsString({ optional: true })
   phoneNumber?: string;

   @IsString({ optional: true, email: true })
   email?: string;
}

export class UpdateStaffDto extends IPartialType(CreateStaffDto) {}
