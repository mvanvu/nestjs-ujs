import { AvailableStatus } from '.prisma/content';
import { UserRefEntity, BaseEntity, StringSchema, EnumSchema, ClassSchema } from '@shared-library';
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

   @ClassSchema(UserRefEntity, { optional: true })
   author?: UserRefEntity;

   @ClassSchema(UserRefEntity, { optional: true })
   editor?: UserRefEntity;

   @StringSchema({ format: 'date-time' })
   createdAt: Date;

   @StringSchema({ format: 'date-time' })
   updatedAt?: Date;

   @ClassSchema(CategoryRef, { optional: true })
   category?: CategoryRef;

   @ClassSchema(MetadataEntity, { optional: true })
   metadata?: MetadataEntity;
}
