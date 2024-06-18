import { AvailableStatus } from '.prisma/order';
import { Property } from '@lib/decorator/property';
import { BaseEntity } from '@lib/entity/base';
import { UserRefEntity } from '@service/user/entity/user';

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
