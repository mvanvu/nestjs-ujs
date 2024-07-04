import { AvailableStatus } from '.prisma/content';
import {
   EnumSchema,
   IPartialType,
   ClassSchema,
   StringSchema,
   UserRefEntity,
   IDSchema,
   EmailSchema,
   NameSchema,
   ImageSchema,
} from '@shared-library';

export class CreateRestaurantDto {
   @IDSchema()
   ownerId: string;

   @ClassSchema(UserRefEntity, { swagger: { readOnly: true } })
   owner: UserRefEntity;

   @EnumSchema(Object.values(AvailableStatus), { optional: true })
   status?: AvailableStatus;

   @NameSchema()
   name: string;

   @StringSchema({ optional: true, transform: 'safeHtml' })
   description?: string;

   @ImageSchema({ optional: true })
   imageUrl?: string;

   @StringSchema({ optional: true })
   address?: string;

   @StringSchema({ optional: true })
   phoneNumber?: string;

   @EmailSchema({ optional: true })
   email?: string;
}

export class UpdateRestaurantDto extends IPartialType(CreateRestaurantDto) {}
