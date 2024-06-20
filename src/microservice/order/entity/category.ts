import { AvailableStatus } from '.prisma/order';
import { BaseEntity, Property, UserRefEntity } from '@shared-library';

export class CategoryEntity extends BaseEntity {
   @Property()
   id: string;

   @Property({ swagger: { enum: Object.values(AvailableStatus) } })
   status: AvailableStatus;

   @Property()
   name: string;

   @Property({ swagger: { type: UserRefEntity } })
   author?: UserRefEntity;

   @Property({ swagger: { type: UserRefEntity } })
   editor?: UserRefEntity;

   @Property()
   createdAt: Date;

   @Property()
   updatedAt?: Date;
}

export class CategoryRef extends BaseEntity {
   @Property()
   id: string;

   @Property()
   name: string;
}
