import { AvailableStatus } from '.prisma/content';
import { EnumSchema, IPartialType, ObjectSchema, StringSchema, UserRefEntity } from '@shared-library';

export class CreateRestaurantDto {
   @StringSchema({ format: 'mongoId' })
   ownerId: string;

   @ObjectSchema(UserRefEntity, { swagger: { readOnly: true } })
   owner: UserRefEntity;

   @EnumSchema(Object.values(AvailableStatus), { optional: true })
   status?: AvailableStatus;

   @StringSchema({ notEmpty: true })
   name: string;

   @StringSchema({ optional: true, transform: 'safeHtml' })
   description?: string;

   @StringSchema({ optional: true, format: 'url' })
   imageUrl?: string;

   @StringSchema({ optional: true })
   address?: string;

   @StringSchema({ optional: true })
   phoneNumber?: string;

   @StringSchema({ optional: true, format: 'email' })
   email?: string;
}

export class UpdateRestaurantDto extends IPartialType(CreateRestaurantDto) {}
