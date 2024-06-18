import { AvailableStatus } from '.prisma/content';
import { IPickType } from '@lib';
import { Property } from '@lib/decorator/property';
import { BaseEntity } from '@lib/entity/base';
import { UserRefEntity } from '@service/user/entity/user';

export class RestaurantEntity extends BaseEntity {
   @Property()
   id: string;

   @Property({ swagger: { type: UserRefEntity } })
   owner: UserRefEntity;

   @Property({ swagger: { enum: Object.values(AvailableStatus) } })
   status: AvailableStatus;

   @Property()
   name: string;

   @Property()
   description?: string;

   @Property()
   address?: string;

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

export class RestaurantRefEntity extends IPickType(RestaurantEntity, ['id', 'name', 'owner']) {}
