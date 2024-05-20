import { RoleStatus, Role } from '.prisma/user';
import { Property } from '@lib/common/decorator';
import { BaseEntity } from '@lib/common/entity';

export class RoleEntity extends BaseEntity {
   @Property()
   id: string;

   @Property()
   name: string;

   @Property()
   status: RoleStatus;

   @Property()
   createdAt?: Date;

   @Property()
   updatedAt?: Date;

   @Property({
      swagger: { example: { id: '6631e55d89af4ff2b9b51aa3', email: 'john.due@email.com', username: 'john' } },
   })
   author?: { id: string; email: string; username: string };

   @Property({
      swagger: { example: { id: '6631e55d89af4ff2b9b51aa3', email: 'john.due@email.com', username: 'john' } },
   })
   editor?: { id: string; email: string; username: string };

   @Property()
   permissions: string[];

   @Property()
   totalUsers?: number;

   constructor(role?: Role & { _count: { userRoles: number } }) {
      super(role);
      this.totalUsers = role?._count?.userRoles ?? 0;
   }
}
