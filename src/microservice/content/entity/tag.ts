import { AvailableStatus } from '.prisma/content';
import { UserRefEntity, EnumSchema, ClassSchema, DateSchema, IDSchema, NameSchema } from '@shared-library';

export class TagEntity {
   @IDSchema()
   id: string;

   @EnumSchema(Object.values(AvailableStatus))
   status: AvailableStatus;

   @NameSchema()
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
