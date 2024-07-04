import { AvailableStatus } from '.prisma/order';
import { EnumSchema, ClassSchema, StringSchema, UserRefEntity, DateSchema, IDSchema } from '@shared-library';

export class CategoryEntity {
   @IDSchema()
   id: string;

   @EnumSchema(Object.values(AvailableStatus))
   status: AvailableStatus;

   @StringSchema()
   name: string;

   @ClassSchema(UserRefEntity, { optional: true })
   author?: UserRefEntity;

   @ClassSchema(UserRefEntity, { optional: true })
   editor?: UserRefEntity;

   @DateSchema()
   createdAt: Date;

   @DateSchema()
   updatedAt: Date;
}

export class CategoryRef {
   @IDSchema()
   id: string;

   @StringSchema()
   name: string;
}
