import { $Enums, Role } from '.prisma/user';
import { EntityProperty } from '@lib/common/decorator';
import { BaseEntity } from '@lib/common/entity';

export class UserRefEntity {
   @EntityProperty()
   id: string;

   @EntityProperty()
   email: string;

   @EntityProperty()
   username: string;
}

export class RoleEntity extends BaseEntity {
   @EntityProperty()
   id: string;

   @EntityProperty()
   name: string;

   @EntityProperty({ swagger: { type: $Enums.RoleStatus, enum: $Enums.RoleStatus } })
   status: $Enums.RoleStatus;

   @EntityProperty()
   createdAt?: Date;

   @EntityProperty()
   updatedAt?: Date;

   @EntityProperty({ swagger: { type: UserRefEntity } })
   author?: { id: string; email: string; username: string };

   @EntityProperty({ swagger: { type: UserRefEntity } })
   editor?: { id: string; email: string; username: string };

   @EntityProperty()
   permissions: string[];

   @EntityProperty()
   totalUsers?: number;

   constructor(role?: Role & { _count: { userRoles: number } }) {
      super(role);
      this.totalUsers = role?._count?.userRoles ?? 0;
   }
}
