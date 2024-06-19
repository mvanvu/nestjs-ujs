import { AvailableStatus } from '.prisma/content';
import { Property } from '@lib/common/decorator/property';
import { UserRefEntity } from '@lib/microservice/user/entity/user';
import { BaseEntity } from '@lib/common/entity/base';
import { CategoryRef } from './category';

export class ToppingEntity extends BaseEntity {
   @Property()
   name: string;

   @Property()
   price: number;
}

export class ItemEntity extends BaseEntity {
   @Property()
   id: string;

   @Property({ swagger: { enum: Object.values(AvailableStatus) } })
   status: AvailableStatus;

   @Property()
   name: string;

   @Property()
   imageUrl?: string;

   @Property()
   basePrice: number;

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

   @Property({ swagger: { type: ToppingEntity } })
   topping?: ToppingEntity;
}
