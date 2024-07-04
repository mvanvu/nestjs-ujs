import { AvailableStatus } from '.prisma/content';
import { UserRefEntity, StringSchema, EnumSchema, ClassSchema, DateSchema } from '@shared-library';
import { MetadataEntity } from './metadata';

export class CategoryRef {
   @StringSchema()
   id: string;

   @StringSchema()
   name: string;

   @StringSchema()
   path: string;
}

export class CategoryEntity {
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

   @ClassSchema(UserRefEntity, { optional: true })
   author?: UserRefEntity;

   @ClassSchema(UserRefEntity, { optional: true })
   editor?: UserRefEntity;

   @DateSchema()
   createdAt: Date;

   @DateSchema()
   updatedAt?: Date;

   @ClassSchema(CategoryRef, { optional: true })
   parent?: CategoryRef;

   @ClassSchema(MetadataEntity, { optional: true })
   metadata?: MetadataEntity;
}
