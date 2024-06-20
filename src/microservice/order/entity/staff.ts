import { StaffStatus } from '.prisma/order';
import { BaseEntity, Property, UserRefEntity } from '@shared-library';

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

   @Property({ swagger: { type: UserRefEntity } })
   author?: UserRefEntity;

   @Property({ swagger: { type: UserRefEntity } })
   editor?: UserRefEntity;

   @Property()
   createdAt: Date;

   @Property()
   updatedAt?: Date;
}
