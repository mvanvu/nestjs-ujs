import { AvailableStatus } from '.prisma/content';
import { BaseEntity, UserRefEntity, StringSchema, EnumSchema, ObjectSchema } from '@shared-library';

export class TagEntity extends BaseEntity {
   @StringSchema()
   id: string;

   @EnumSchema(Object.values(AvailableStatus))
   status: AvailableStatus;

   @StringSchema()
   name: string;

   @ObjectSchema(UserRefEntity, { optional: true })
   author?: UserRefEntity;

   @ObjectSchema(UserRefEntity, { optional: true })
   editor?: UserRefEntity;

   @StringSchema({ format: 'date-time' })
   createdAt: Date;

   @StringSchema({ format: 'date-time' })
   updatedAt: Date;
}
