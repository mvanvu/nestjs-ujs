import { AvailableStatus } from '.prisma/content';
import { Property } from '@lib/common/decorator/property';
import { BaseEntity } from '@lib/common/entity/base';
import { MetadataEntity } from './metadata';
import { UserRefEntity } from '@lib/common/entity/user';

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
