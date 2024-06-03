import { $Enums, Group } from '.prisma/user';
import { IProperty } from '@lib/common/decorator/property';
import { BaseEntity } from '@lib/common/entity/base';

export class RoleGroupEntity {
   @IProperty()
   id: string;

   @IProperty()
   name: string;

   @IProperty()
   permissions: string[];
}

export class ChildrenGroupEntity {
   @IProperty()
   id: string;

   @IProperty()
   name: string;

   @IProperty({ swagger: { type: [RoleGroupEntity] } })
   roles: RoleGroupEntity[];
}

export class GroupEntity extends BaseEntity {
   @IProperty()
   id: string;

   @IProperty({ swagger: { type: $Enums.AvailableStatus, enum: $Enums.AvailableStatus } })
   status: $Enums.AvailableStatus;

   @IProperty()
   name: string;

   @IProperty()
   description: string;

   @IProperty()
   createdAt?: Date;

   @IProperty()
   createdBy?: string;

   @IProperty()
   updatedAt?: Date;

   @IProperty()
   updatedBy?: string;

   @IProperty({ swagger: { type: [ChildrenGroupEntity] } })
   groups: ChildrenGroupEntity[];

   @IProperty({ swagger: { type: [RoleGroupEntity] } })
   roles: RoleGroupEntity[];

   @IProperty()
   totalActiveUsers: number;

   constructor(record?: Group & { _count: { users: number } }) {
      super(record);

      if (record?._count?.users !== undefined) {
         this.totalActiveUsers = record._count.users;
      }
   }
}
