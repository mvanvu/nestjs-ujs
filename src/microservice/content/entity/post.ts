import { AvailableStatus } from '.prisma/content';
import { UserRefEntity, StringSchema, EnumSchema, ClassSchema, DateSchema } from '@shared-library';
import { CategoryRef } from './category';
import { MetadataEntity } from './metadata';

export class PostEntity {
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

   @DateSchema()
   createdAt: Date;

   @DateSchema()
   updatedAt?: Date;

   @ClassSchema(CategoryRef, { optional: true })
   category?: CategoryRef;

   @ClassSchema(MetadataEntity, { optional: true })
   metadata?: MetadataEntity;
}
