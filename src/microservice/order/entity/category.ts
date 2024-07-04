import { AvailableStatus } from '.prisma/order';
import { EnumSchema, ClassSchema, StringSchema, UserRefEntity, DateSchema } from '@shared-library';

export class CategoryEntity {
   @StringSchema()
   id: string;

   @EnumSchema(Object.values(AvailableStatus))
   status: AvailableStatus;

   @StringSchema()
   name: string;

   @ClassSchema(UserRefEntity)
   author?: UserRefEntity;

   @ClassSchema(UserRefEntity)
   editor?: UserRefEntity;

   @DateSchema()
   createdAt: Date;

   @DateSchema()
   updatedAt?: Date;
}

export class CategoryRef {
   @StringSchema()
   id: string;

   @StringSchema()
   name: string;
}
