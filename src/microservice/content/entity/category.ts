import { AvailableStatus } from '.prisma/content';
import { BaseEntity, UserRefEntity, StringSchema, EnumSchema, ObjectSchema } from '@shared-library';
import { MetadataEntity } from './metadata';

export class CategoryRef extends BaseEntity {
   @StringSchema()
   id: string;

   @StringSchema()
   name: string;

   @StringSchema()
   path: string;
}

export class CategoryEntity extends BaseEntity {
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

   @ObjectSchema(UserRefEntity, { optional: true })
   author?: UserRefEntity;

   @ObjectSchema(UserRefEntity, { optional: true })
   editor?: UserRefEntity;

   @StringSchema({ format: 'date-time' })
   createdAt: Date;

   @StringSchema({ format: 'date-time' })
   updatedAt?: Date;

   @ObjectSchema(CategoryRef, { optional: true })
   parent?: CategoryRef;

   @ObjectSchema(MetadataEntity, { optional: true })
   metadata?: MetadataEntity;
}
