import { RoleStatus, Role } from '.prisma/user';
import { EntityProperty } from '@lib/common/decorator';
import { BaseEntity } from '@lib/common/entity';
import { ApiProperty } from '@nestjs/swagger';

export class UserRefEntity {
   @ApiProperty()
   id: string;

   @ApiProperty()
   email: string;

   @ApiProperty()
   username: string;
}

export class RoleEntity extends BaseEntity {
   @EntityProperty()
   id: string;

   @EntityProperty()
   name: string;

   @EntityProperty()
   status: RoleStatus;

   @EntityProperty()
   createdAt?: Date;

   @EntityProperty()
   updatedAt?: Date;

   @ApiProperty({ type: () => UserRefEntity })
   @EntityProperty()
   author?: { id: string; email: string; username: string };

   @ApiProperty({ type: () => UserRefEntity })
   @EntityProperty()
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
