import { appConfig } from '@config';
import { BaseEntity, IProperty, PermissionOptions, PaginationMeta } from '@lib';
import { Is, ObjectRecord } from '@mvanvu/ujs';

export class UserEntity extends BaseEntity {
   @IProperty()
   id: string;

   @IProperty()
   name?: string;

   @IProperty()
   username?: string;

   @IProperty()
   email: string;

   @IProperty()
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
   @IProperty()
   access: string;

   @IProperty()
   refresh: string;
}

export class AuthEntity extends BaseEntity {
   @IProperty()
   user: UserEntity;

   @IProperty()
   tokens: AuthTokens;
}

export class PaginationUserEntity {
   @IProperty({ swagger: { isArray: true } })
   data: UserEntity[];

   @IProperty()
   meta: PaginationMeta;
}
