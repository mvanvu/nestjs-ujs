import { $Enums } from '.prisma/user';
import { Property } from '@lib/decorator';
import { BaseEntity } from '@lib/entity';

export class RoleEntity extends BaseEntity {
   @Property()
   id: string;

   @Property({ swagger: { type: $Enums.AvailableStatus, enum: $Enums.AvailableStatus } })
   status: $Enums.AvailableStatus;

   @Property()
   name: string;

   @Property()
   description: string;

   @Property()
   createdAt?: Date;

   @Property()
   updatedAt?: Date;

   @Property()
   permissions: string[];
}
