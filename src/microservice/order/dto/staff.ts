import { StaffStatus } from '.prisma/order';
import {
   EnumSchema,
   IPartialType,
   StringSchema,
   IDSchema,
   EmailSchema,
   NameSchema,
   ImageSchema,
} from '@shared-library';

export class CreateStaffDto {
   @IDSchema()
   restaurantId: string;

   @EnumSchema(Object.values(StaffStatus), { optional: true })
   status?: StaffStatus;

   @NameSchema({ optional: true })
   name: string;

   @ImageSchema({ optional: true })
   imageUrl?: string;

   @StringSchema({ optional: true })
   phoneNumber?: string;

   @EmailSchema({ optional: true })
   email?: string;
}

export class UpdateStaffDto extends IPartialType(CreateStaffDto) {}
