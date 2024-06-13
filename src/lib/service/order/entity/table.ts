import { AvailableStatus, TableStatus } from '.prisma/order';
import { Property } from '@lib/common/decorator/property';
import { BaseEntity } from '@lib/common/entity/base';
import { UserRefEntity } from '@lib/service/user/entity/user';
import { RestaurantRefEntity } from './restaurant';

export class TableEntity extends BaseEntity {
   @Property()
   id: string;

   @Property({ swagger: { type: RestaurantRefEntity } })
   restaurant: RestaurantRefEntity;

   @Property({ swagger: { enum: Object.values(AvailableStatus) } })
   status: AvailableStatus;

   @Property({ swagger: { enum: Object.values(TableStatus) } })
   activityStatus?: TableStatus;

   @Property()
   number: number;

   @Property()
   area?: string;

   @Property({ swagger: { type: UserRefEntity } })
   author?: UserRefEntity;

   @Property({ swagger: { type: UserRefEntity } })
   editor?: UserRefEntity;

   @Property()
   createdAt: Date;

   @Property()
   updatedAt?: Date;
}
