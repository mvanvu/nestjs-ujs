import { appConfig } from '@config';
import { Is, ObjectRecord } from '@mvanvu/ujs';
import { UserStatus } from '.prisma/user';
import { EntityProperty } from '@lib/common/decorator/property';
import { BaseEntity } from '@lib/common/entity/base';
import { PermissionOptions } from '@lib/common/type/common';

export class RoleRefEntity {
   @EntityProperty()
   id: string;

   @EntityProperty()
   name: string;

   @EntityProperty()
   permissions: string[];
}

export class UserEntity extends BaseEntity {
   @EntityProperty()
   id: string;

   @EntityProperty({ swagger: { type: UserStatus, enum: UserStatus } })
   status: UserStatus;

   @EntityProperty()
   name?: string;

   @EntityProperty()
   username?: string;

   @EntityProperty()
   email: string;

   @EntityProperty({ swagger: { type: [RoleRefEntity] } })
   roles: RoleRefEntity[];

   @EntityProperty()
   createdAt: Date;

   @EntityProperty()
   createdBy: string;

   @EntityProperty()
   updatedAt: Date;

   @EntityProperty()
   updatedBy: string;

   constructor(entity?: ObjectRecord) {
      super(entity);

      if (Array.isArray(entity?.userRoles)) {
         this.roles = entity.userRoles.map(({ role }) => role);
      }
   }

   get isRoot(): boolean {
      let isUserRoot: boolean = false;
      const rootUid = appConfig.get('rootUid');

      if (rootUid) {
         if (Is.email(rootUid)) {
            isUserRoot = this.email === rootUid;
         } else {
            isUserRoot = this.id === rootUid || (this.username && this.username === rootUid);
         }
      }

      return isUserRoot;
   }

   authorise(permission?: PermissionOptions): boolean {
      const isUserRoot = this.isRoot;

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

   compare(user: UserEntity, permission: PermissionOptions): number {
      // Check the same root
      const isSelfRoot = this.isRoot;
      const isUserRoot = user.isRoot;

      if (isSelfRoot && isUserRoot) {
         return 0;
      }

      if (isSelfRoot) {
         return 1;
      }

      if (isUserRoot) {
         return -1;
      }

      // Check the same permission
      const hasSelfPermission = this.authorise(permission);
      const hasUserPermission = user.authorise(permission);

      if (hasSelfPermission && hasUserPermission) {
         return 0;
      }

      if (hasSelfPermission) {
         return 1;
      }

      if (hasUserPermission) {
         return -1;
      }
   }
}

export class AuthTokens {
   @EntityProperty()
   access: string;

   @EntityProperty()
   refresh: string;
}

export class AuthEntity extends BaseEntity {
   @EntityProperty()
   user: UserEntity;

   @EntityProperty()
   tokens: AuthTokens;
}
