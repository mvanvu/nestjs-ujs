import { AvailableStatus } from '.prisma/order';
import { Property } from '@lib/common/decorator/property';
import { BaseEntity } from '@lib/common/entity/base';
import { UserRefEntity } from '@lib/common/entity/user-ref';

export class CategoryEntity extends BaseEntity {
   @Property()
   id: string;

   @Property()
   status: AvailableStatus;

   @Property()
   name: string;

   @Property()
   author?: UserRefEntity;

   @Property()
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
