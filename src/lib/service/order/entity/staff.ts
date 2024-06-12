import { StaffStatus } from '.prisma/order';
import { Property } from '@lib/common/decorator/property';
import { BaseEntity } from '@lib/common/entity/base';
import { UserRefEntity } from '@lib/common/entity/user-ref';

export class StaffEntity extends BaseEntity {
   @Property()
   id: string;

   @Property({ swagger: { enum: Object.values(StaffStatus) } })
   status: StaffStatus;

   @Property()
   name: string;

   @Property()
   phoneNumber?: string;

   @Property()
   email?: string;

   @Property()
   author?: UserRefEntity;

   @Property()
   editor?: UserRefEntity;

   @Property()
   createdAt: Date;

   @Property()
   updatedAt?: Date;
}
