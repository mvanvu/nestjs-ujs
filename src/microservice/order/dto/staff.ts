import { StaffStatus } from '.prisma/order';
import { EnumSchema, IPartialType, StringSchema } from '@shared-library';

export class CreateStaffDto {
   @StringSchema({ format: 'mongoId' })
   restaurantId: string;

   @EnumSchema(Object.values(StaffStatus), { optional: true })
   status?: StaffStatus;

   @StringSchema({ optional: true, empty: false })
   name: string;

   @StringSchema({ optional: true, format: 'url' })
   imageUrl?: string;

   @StringSchema({ optional: true })
   phoneNumber?: string;

   @StringSchema({ optional: true, format: 'email' })
   email?: string;
}

export class UpdateStaffDto extends IPartialType(CreateStaffDto) {}
