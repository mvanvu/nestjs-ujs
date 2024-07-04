import { StaffStatus } from '.prisma/order';
import {
   EnumSchema,
   ClassSchema,
   StringSchema,
   UserRefEntity,
   DateSchema,
   IDSchema,
   EmailSchema,
} from '@shared-library';

export class StaffEntity {
   @IDSchema()
   id: string;

   @EnumSchema(Object.values(StaffStatus))
   status: StaffStatus;

   @StringSchema()
   name: string;

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
