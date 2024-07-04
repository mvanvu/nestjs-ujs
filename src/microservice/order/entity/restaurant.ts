import { AvailableStatus } from '.prisma/content';
import {
   EnumSchema,
   IPickType,
   ClassSchema,
   StringSchema,
   UserRefEntity,
   DateSchema,
   EmailSchema,
} from '@shared-library';

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

   @EmailSchema({ optional: true })
   email?: string;

   @ClassSchema(UserRefEntity, { optional: true })
   author?: UserRefEntity;

   @ClassSchema(UserRefEntity, { optional: true })
   editor?: UserRefEntity;

   @DateSchema()
   createdAt: Date;

   @DateSchema()
   updatedAt: Date;
}

export class RestaurantRefEntity extends IPickType(RestaurantEntity, ['id', 'name', 'owner']) {}
