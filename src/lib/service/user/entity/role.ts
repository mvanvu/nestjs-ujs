import { BaseEntity, Property } from '@lib';
import { RoleStatus } from '.prisma/user';

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
}
