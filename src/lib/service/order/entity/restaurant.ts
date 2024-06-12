import { AvailableStatus } from '.prisma/content';
import { Property } from '@lib/common/decorator/property';
import { BaseEntity } from '@lib/common/entity/base';
import { UserRefEntity } from '@lib/common/entity/user-ref';

export class RestaurantEntity extends BaseEntity {
   @Property()
   id: string;

   @Property()
   owner: UserRefEntity;

   @Property()
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

   @Property()
   author?: UserRefEntity;

   @Property()
   editor?: UserRefEntity;

   @Property()
   createdAt: Date;

   @Property()
   updatedAt?: Date;
}
