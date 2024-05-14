import { appConfig } from '@config';
import { BaseEntity, Property, PermissionOptions } from '@lib';
import { Is, ObjectRecord } from '@mvanvu/ujs';

export class UserEntity extends BaseEntity {
   @Property()
   id: string;

   @Property()
   name?: string;

   @Property()
   username?: string;

   @Property()
   email: string;

   @Property()
   roles: { id: string; name: string; permissions: string[] }[];

   constructor(entity?: ObjectRecord) {
      super(entity);

      if (Array.isArray(entity?.userRoles)) {
         this.roles = entity.userRoles.map(({ role }) => role);
      }
   }

   authorise(permission?: PermissionOptions): boolean {
      let isUserRoot: boolean = false;
      const { rootUid } = appConfig;

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
