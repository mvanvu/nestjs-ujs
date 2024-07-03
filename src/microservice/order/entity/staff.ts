import { StaffStatus } from '.prisma/order';
import { EnumSchema, ClassSchema, StringSchema, UserRefEntity } from '@shared-library';

export class StaffEntity {
   @StringSchema()
   id: string;

   @EnumSchema(Object.values(StaffStatus))
   status: StaffStatus;

   @StringSchema()
   name: string;

   @StringSchema({ optional: true })
   phoneNumber?: string;

   @StringSchema({ optional: true })
   email?: string;

   @ClassSchema(UserRefEntity, { optional: true })
   author?: UserRefEntity;

   @ClassSchema(UserRefEntity, { optional: true })
   editor?: UserRefEntity;

   @StringSchema({ format: 'date-time' })
   createdAt: Date;

   @StringSchema({ format: 'date-time' })
   updatedAt: Date;
}
