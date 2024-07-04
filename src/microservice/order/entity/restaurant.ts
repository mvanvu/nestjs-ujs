import { AvailableStatus } from '.prisma/content';
import { EnumSchema, IPickType, ClassSchema, StringSchema, UserRefEntity, DateSchema } from '@shared-library';

export class RestaurantEntity {
   @StringSchema()
   id: string;

   @ClassSchema(UserRefEntity)
   owner: UserRefEntity;

   @EnumSchema(Object.values(AvailableStatus))
   status: AvailableStatus;

   @StringSchema()
   name: string;

   @StringSchema({ optional: true })
   description?: string;

   @StringSchema({ optional: true })
   address?: string;

   @StringSchema({ optional: true })
   phoneNumber?: string;

   @StringSchema({ optional: true, format: 'email' })
   email?: string;

   @ClassSchema(UserRefEntity)
   author?: UserRefEntity;

   @ClassSchema(UserRefEntity)
   editor?: UserRefEntity;

   @DateSchema()
   createdAt: Date;

   @DateSchema()
   updatedAt?: Date;
}

export class RestaurantRefEntity extends IPickType(RestaurantEntity, ['id', 'name', 'owner']) {}
