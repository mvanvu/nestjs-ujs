import { AvailableStatus } from '.prisma/content';
import { Property, BaseEntity, UserRefEntity } from '@shared-library';
import { MetadataEntity } from './metadata';

export class CategoryRef extends BaseEntity {
   @Property()
   id: string;

   @Property()
   name: string;

   @Property()
   path: string;
}

export class CategoryEntity extends BaseEntity {
   @Property()
   id: string;

   @Property({ swagger: { enum: Object.values(AvailableStatus) } })
   status: AvailableStatus;

   @Property()
   name: string;

   @Property()
   path: string;

   @Property()
   description?: string;

   @Property({ swagger: { type: UserRefEntity } })
   author?: UserRefEntity;

   @Property({ swagger: { type: UserRefEntity } })
   editor?: UserRefEntity;

   @Property()
   createdAt: Date;

   @Property()
   updatedAt?: Date;

   @Property({ swagger: { type: CategoryRef } })
   parent?: CategoryRef;

   @Property({ swagger: { type: MetadataEntity } })
   metadata?: MetadataEntity;
}
