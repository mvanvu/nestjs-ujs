import { AvailableStatus } from '.prisma/content';
import { Property } from '@lib/decorator/property';
import { UserRefEntity } from '@service/user/entity/user';
import { BaseEntity } from '@lib/entity/base';
import { CategoryRef } from './category';
import { MetadataEntity } from './metadata';

export class PostEntity extends BaseEntity {
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

   @Property()
   imageUrl?: string;

   @Property({ swagger: { type: UserRefEntity } })
   author?: UserRefEntity;

   @Property({ swagger: { type: UserRefEntity } })
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
