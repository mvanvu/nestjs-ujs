import { AvailableStatus, TableStatus } from '.prisma/order';
import { Property } from '@lib/common/decorator/property';
import { BaseEntity } from '@lib/common/entity/base';
import { UserRefEntity } from '@lib/common/entity/user-ref';

export class TableEntity extends BaseEntity {
   @Property()
   id: string;

   @Property()
   status: AvailableStatus;

   @Property({ swagger: { enum: Object.values(TableStatus) } })
   activityStatus?: TableStatus;

   @Property()
   number: number;

   @Property()
   area?: string;

   @Property()
   author?: UserRefEntity;

   @Property()
   editor?: UserRefEntity;

   @Property()
   createdAt: Date;

   @Property()
   updatedAt?: Date;
}
