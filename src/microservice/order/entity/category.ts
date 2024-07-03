import { AvailableStatus } from '.prisma/order';
import { BaseEntity, EnumSchema, ClassSchema, StringSchema, UserRefEntity } from '@shared-library';

export class CategoryEntity extends BaseEntity {
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

   @StringSchema({ format: 'date-time' })
   createdAt: Date;

   @StringSchema({ format: 'date-time' })
   updatedAt?: Date;
}

export class CategoryRef extends BaseEntity {
   @StringSchema()
   id: string;

   @StringSchema()
   name: string;
}
