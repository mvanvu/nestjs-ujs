import { RoleStatus, Role } from '.prisma/user';
import { EntityProperty } from '@lib/common/decorator';
import { BaseEntity } from '@lib/common/entity';
import { ApiProperty } from '@nestjs/swagger';

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

   @ApiProperty({ example: { id: '6631e55d89af4ff2b9b51aa3', email: 'john.due@email.com', username: 'john' } })
   @EntityProperty()
   author?: { id: string; email: string; username: string };

   @ApiProperty({ example: { id: '6631e55d89af4ff2b9b51aa3', email: 'john.due@email.com', username: 'john' } })
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
