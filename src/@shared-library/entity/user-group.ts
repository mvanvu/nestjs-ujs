import { $Enums, Group } from '.prisma/user';
import { EnumSchema, NumberSchema, ClassSchema, StringSchema } from '../decorator/schema';
import { BaseEntity } from './base';

export class RoleGroupEntity {
   @StringSchema()
   id: string;

   @StringSchema()
   name: string;

   @StringSchema({ each: true })
   permissions: string[];
}

export class ChildrenGroupEntity {
   @StringSchema()
   id: string;

   @StringSchema()
   name: string;

   @ClassSchema(RoleGroupEntity, { each: true })
   roles: RoleGroupEntity[];
}

export class GroupEntity extends BaseEntity {
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

   @ClassSchema(ChildrenGroupEntity, { each: true })
   groups: ChildrenGroupEntity[];

   @ClassSchema(RoleGroupEntity, { each: true })
   roles: RoleGroupEntity[];

   @NumberSchema({ integer: true, min: 0 })
   totalActiveUsers: number;

   constructor(record?: Group & { _count: { users: number } }) {
      super(record);

      if (record?._count?.users !== undefined) {
         this.totalActiveUsers = record._count.users;
      }
   }
}
