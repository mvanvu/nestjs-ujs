import { AvailableStatus } from '.prisma/content';
import { BaseEntity, EnumSchema, IPickType, ObjectSchema, StringSchema, UserRefEntity } from '@shared-library';

export class RestaurantEntity extends BaseEntity {
   @StringSchema()
   id: string;

   @ObjectSchema(UserRefEntity)
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

   @ObjectSchema(UserRefEntity)
   author?: UserRefEntity;

   @ObjectSchema(UserRefEntity)
   editor?: UserRefEntity;

   @StringSchema({ format: 'date-time' })
   createdAt: Date;

   @StringSchema({ format: 'date-time' })
   updatedAt?: Date;
}

export class RestaurantRefEntity extends IPickType(RestaurantEntity, ['id', 'name', 'owner']) {}
