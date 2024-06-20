import { $Enums } from '.prisma/user';
import { Property } from '../decorator/property';
import { BaseEntity } from './base';

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
