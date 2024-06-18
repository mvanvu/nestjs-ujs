import { $Enums, Group } from '.prisma/user';
import { Property } from '@lib/decorator/property';
import { BaseEntity } from '@lib/entity/base';

export class RoleGroupEntity {
   @Property()
   id: string;

   @Property()
   name: string;

   @Property()
   permissions: string[];
}

export class ChildrenGroupEntity {
   @Property()
   id: string;

   @Property()
   name: string;

   @Property({ swagger: { type: [RoleGroupEntity] } })
   roles: RoleGroupEntity[];
}

export class GroupEntity extends BaseEntity {
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
   createdBy?: string;

   @Property()
   updatedAt?: Date;

   @Property()
   updatedBy?: string;

   @Property({ swagger: { type: [ChildrenGroupEntity] } })
   groups: ChildrenGroupEntity[];

   @Property({ swagger: { type: [RoleGroupEntity] } })
   roles: RoleGroupEntity[];

   @Property()
   totalActiveUsers: number;

   constructor(record?: Group & { _count: { users: number } }) {
      super(record);

      if (record?._count?.users !== undefined) {
         this.totalActiveUsers = record._count.users;
      }
   }
}
