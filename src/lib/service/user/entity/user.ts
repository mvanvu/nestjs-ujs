import { appConfig } from '@config';
import { Property } from '@lib/common/decorator';
import { BaseEntity } from '@lib/common/entity';
import { PermissionOptions } from '@lib/common/type';
import { Is, ObjectRecord } from '@mvanvu/ujs';
import { UserStatus } from '.prisma/user';

export class UserEntity extends BaseEntity {
   @Property()
   id: string;

   @Property()
   status: UserStatus;

   @Property()
   name?: string;

   @Property()
   username?: string;

   @Property()
   email: string;

   @Property({
      swagger: {
         example: { id: '6631e55d89af4ff2b9b51aa3', name: 'Admin role' },
         isArray: true,
      },
   })
   roles: { id: string; name: string; permissions: string[] }[];

   constructor(entity?: ObjectRecord) {
      super(entity);

      if (Array.isArray(entity?.userRoles)) {
         this.roles = entity.userRoles.map(({ role }) => role);
      }
   }

   authorise(permission?: PermissionOptions): boolean {
      let isUserRoot: boolean = false;
      const rootUid = appConfig.get('rootUid');

      if (rootUid) {
         if (Is.email(rootUid)) {
            isUserRoot = this.email === rootUid;
         } else {
            isUserRoot = this.id === rootUid || (this.username && this.username === rootUid);
         }
      }

      if ((!permission || Is.emptyObject(permission)) && !isUserRoot) {
         return false;
      }

      if (isUserRoot) {
         return true;
      }

      if (!this.roles?.length) {
         return false;
      }

      const userPermissions: string[] = [];
      this.roles.forEach((role) => userPermissions.push(...role.permissions));

      if (typeof permission === 'string') {
         permission = { key: permission };
      }

      if (
         !userPermissions.length ||
         (permission.key && !userPermissions.includes(permission.key)) ||
         (permission.or?.length && !userPermissions.find((permit) => permission.or.includes(permit))) ||
         (permission.and?.length &&
            userPermissions.filter((permit) => permission.and.includes(permit)).length < permission.and.length)
      ) {
         return false;
      }

      return true;
   }
}

export class AuthTokens {
   @Property()
   access: string;

   @Property()
   refresh: string;
}

export class AuthEntity extends BaseEntity {
   @Property()
   user: UserEntity;

   @Property()
   tokens: AuthTokens;
}
