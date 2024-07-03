import { $Enums, Group } from '.prisma/user';
import { EnumSchema, NumberSchema, ClassSchema, StringSchema } from '../decorator/schema';

export class RoleGroupEntity {
   @StringSchema()
   id: string;

   @StringSchema()
   name: string;

   @StringSchema({ isArray: true })
   permissions: string[];
}

export class ChildrenGroupEntity {
   @StringSchema()
   id: string;

   @StringSchema()
   name: string;

   @ClassSchema(RoleGroupEntity, { isArray: true })
   roles: RoleGroupEntity[];
}

export class GroupEntity {
   @StringSchema()
   id: string;

   @EnumSchema(Object.values($Enums.AvailableStatus))
   status: $Enums.AvailableStatus;

   @StringSchema()
   name: string;

   @StringSchema()
   description: string;

   @StringSchema({ format: 'date-time' })
   createdAt?: Date;

   @StringSchema()
   createdBy?: string;

   @StringSchema({ format: 'date-time' })
   updatedAt?: Date;

   @StringSchema()
   updatedBy?: string;

   @ClassSchema(ChildrenGroupEntity, { isArray: true })
   groups: ChildrenGroupEntity[];

   @ClassSchema(RoleGroupEntity, { isArray: true })
   roles: RoleGroupEntity[];

   @NumberSchema({ integer: true, min: 0 })
   totalActiveUsers: number;

   bind(record?: Group & { _count: { users: number } }) {
      if (record?._count?.users !== undefined) {
         this.totalActiveUsers = record._count.users;
      }
   }
}
