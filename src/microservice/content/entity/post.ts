import { AvailableStatus } from '.prisma/content';
import { UserRefEntity, BaseEntity, StringSchema, EnumSchema, ObjectSchema } from '@shared-library';
import { CategoryRef } from './category';
import { MetadataEntity } from './metadata';

export class PostEntity extends BaseEntity {
   @StringSchema()
   id: string;

   @EnumSchema(Object.values(AvailableStatus))
   status: AvailableStatus;

   @StringSchema()
   name: string;

   @StringSchema()
   path: string;

   @StringSchema({ optional: true })
   description?: string;

   @StringSchema({ optional: true, format: 'url' })
   imageUrl?: string;

   @ObjectSchema(UserRefEntity, { optional: true })
   author?: UserRefEntity;

   @ObjectSchema(UserRefEntity, { optional: true })
   editor?: UserRefEntity;

   @StringSchema({ format: 'date-time' })
   createdAt: Date;

   @StringSchema({ format: 'date-time' })
   updatedAt?: Date;

   @ObjectSchema(CategoryRef, { optional: true })
   category?: CategoryRef;

   @ObjectSchema(MetadataEntity, { optional: true })
   metadata?: MetadataEntity;
}
