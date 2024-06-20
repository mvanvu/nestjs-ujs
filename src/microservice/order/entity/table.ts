import { AvailableStatus, TableStatus } from '.prisma/order';
import { RestaurantRefEntity } from './restaurant';
import { BaseEntity, Property, UserRefEntity } from '@shared-library';

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
