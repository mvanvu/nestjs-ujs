import { AvailableStatus } from '.prisma/content';
import {
   UserRefEntity,
   StringSchema,
   EnumSchema,
   ClassSchema,
   DateSchema,
   IDSchema,
   ImageSchema,
   NameSchema,
} from '@shared-library';
import { CategoryRef } from './category';
import { MetadataEntity } from './metadata';

export class PostEntity {
   @IDSchema()
   id: string;

   @EnumSchema(Object.values(AvailableStatus))
   status: AvailableStatus;

   @NameSchema()
   name: string;

   @StringSchema()
   path: string;

   @StringSchema({ optional: true })
   description?: string;

   @ImageSchema({ optional: true })
   imageUrl?: string;

   @ClassSchema(UserRefEntity, { optional: true })
   author?: UserRefEntity;

   @ClassSchema(UserRefEntity, { optional: true })
   editor?: UserRefEntity;

   @DateSchema()
   createdAt: Date;

   @DateSchema()
   updatedAt: Date;

   @ClassSchema(CategoryRef, { optional: true })
   category?: CategoryRef;

   @ClassSchema(MetadataEntity, { optional: true })
   metadata?: MetadataEntity;
}
