import { AvailableStatus } from '.prisma/content';
import { UserRefEntity, StringSchema, EnumSchema, ClassSchema, DateSchema } from '@shared-library';

export class TagEntity {
   @StringSchema()
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
