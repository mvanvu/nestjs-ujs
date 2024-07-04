import { $Enums, Group } from '.prisma/user';
import {
   EnumSchema,
   NumberSchema,
   ClassSchema,
   StringSchema,
   DateSchema,
   IDSchema,
   NameSchema,
} from '../decorator/schema';

export class RoleGroupEntity {
   @IDSchema()
   id: string;

   @NameSchema()
   name: string;

   @StringSchema({ isArray: 'unique' })
   permissions: string[];
}

export class ChildrenGroupEntity {
   @StringSchema()
   id: string;

   @NameSchema()
   name: string;

   @ClassSchema(RoleGroupEntity, { isArray: 'unique' })
   roles: RoleGroupEntity[];
}

export class GroupEntity {
   @IDSchema()
   id: string;

   @EnumSchema(Object.values($Enums.AvailableStatus))
   status: $Enums.AvailableStatus;

   @NameSchema()
   name: string;

   @StringSchema()
   description: string;

   @DateSchema({ optional: true })
   createdAt?: Date;

   @IDSchema({ optional: true })
   createdBy?: string;

   @DateSchema({ optional: true })
   updatedAt?: Date;

   @IDSchema({ optional: true })
   updatedBy?: string;

   @ClassSchema(ChildrenGroupEntity, { isArray: 'unique' })
   groups: ChildrenGroupEntity[];

   @ClassSchema(RoleGroupEntity, { isArray: 'unique' })
   roles: RoleGroupEntity[];

   @NumberSchema({ integer: true, min: 0 })
   totalActiveUsers: number;

   bind(record?: Group & { _count: { users: number } }) {
      if (record?._count?.users !== undefined) {
         this.totalActiveUsers = record._count.users;
      }
   }
}
