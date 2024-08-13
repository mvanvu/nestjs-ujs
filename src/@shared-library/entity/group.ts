import { $Enums, Group } from '.prisma/user';
import { Schema } from '@mvanvu/ujs';

export class RoleGroupEntity {
   @Schema.mongoId().decorate()
   id: string;

   @Schema.content().decorate()
   name: string;

   @Schema.string().array().decorate()
   permissions: string[];
}

export class ChildrenGroupEntity {
   @Schema.mongoId().decorate()
   id: string;

   @Schema.content().decorate()
   name: string;

   @Schema.classRef(RoleGroupEntity).array().decorate()
   roles: RoleGroupEntity[];
}

export class GroupEntity {
   @Schema.mongoId().decorate()
   id: string;

   @Schema.enum(Object.values($Enums.AvailableStatus)).decorate()
   status: $Enums.AvailableStatus;

   @Schema.content().decorate()
   name: string;

   @Schema.string().optional().decorate()
   description?: string;

   @Schema.dateTime().decorate()
   createdAt: Date;

   @Schema.mongoId().optional().decorate()
   createdBy?: string;

   @Schema.dateTime().optional().decorate()
   updatedAt?: Date;

   @Schema.mongoId().optional().decorate()
   updatedBy?: string;

   @Schema.classRef(ChildrenGroupEntity).array().decorate()
   groups: ChildrenGroupEntity[];

   @Schema.classRef(RoleGroupEntity).array().decorate()
   roles: RoleGroupEntity[];

   @Schema.uint().decorate()
   totalActiveUsers: number;

   bind(record?: Group & { _count: { users: number } }) {
      if (record?._count?.users !== undefined) {
         this.totalActiveUsers = record._count.users;
      }
   }
}
