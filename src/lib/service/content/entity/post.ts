import { AvailableStatus } from '.prisma/content';
import { Property } from '@lib/common/decorator/property';
import { UserRefEntity } from '@lib/common/entity/user-ref';
import { BaseEntity } from '@lib/common/entity/base';
import { CategoryRef } from './category';
import { MetadataEntity } from './metadata';

export class PostEntity extends BaseEntity {
   @Property()
   id: string;

   @Property()
   status: AvailableStatus;

   @Property()
   name: string;

   @Property()
   path: string;

   @Property()
   description?: string;

   @Property()
   imageUrl?: string;

   @Property()
   author?: UserRefEntity;

   @Property()
   editor?: UserRefEntity;

   @Property()
   createdAt: Date;

   @Property()
   updatedAt?: Date;

   @Property({ swagger: { type: CategoryRef } })
   category?: CategoryRef;

   @Property({ swagger: { type: MetadataEntity } })
   metadata?: MetadataEntity;
}
