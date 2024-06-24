import { Is } from '@mvanvu/ujs';
import { UserStatus } from '.prisma/user';
import { Property } from '../decorator/property';
import { BaseEntity } from './base';
import { PermissionOptions } from '../type/common';
import { IPickType } from './mapped-type';
import { GroupEntity } from './user-group';
import { USER_PERMISSION_ADMIN_SCOPE } from '@shared-library/constant/common';

export class UserGroupEntity extends IPickType(GroupEntity, ['id', 'name', 'groups', 'roles']) {}
export class UserEntity extends BaseEntity {
   @Property()
   id: string;

   @Property({ swagger: { enum: UserStatus } })
   status: UserStatus;

   @Property()
   name?: string;

   @Property()
   username?: string;

   @Property()
   avatarUrl?: string;

   @Property()
   email: string;

   @Property({ swagger: { type: UserGroupEntity } })
   group?: UserGroupEntity;

   @Property()
   createdAt: Date;

   @Property()
   createdBy: string;

   @Property()
   updatedAt: Date;

   @Property()
   updatedBy: string;

   private _permissions: string[];

   get permissions(): string[] {
      if (!this._permissions) {
         this._permissions = [];

         if (this.group?.roles?.length) {
            for (const { permissions } of this.group.roles) {
               this._permissions.push(...permissions);
            }
         }

         if (this.group?.groups?.length) {
            for (const group of this.group.groups) {
               for (const { permissions } of group.roles) {
                  this._permissions.push(...permissions);
               }
            }
         }

         // Make unique permissions
         this._permissions = Array.from(new Set(this._permissions));
      }

      return this._permissions;
   }

   get isRoot(): boolean {
      let isUserRoot: boolean = false;
      const rootUID = process.env.ROOT_UID || '';

      if (rootUID) {
         if (Is.email(rootUID)) {
            isUserRoot = this.email === rootUID;
         } else {
            isUserRoot = this.id === rootUID || (this.username && this.username === rootUID);
         }
      }

      return isUserRoot;
   }

   authorise(permission?: PermissionOptions): boolean {
      const isUserRoot = this.isRoot;
      const userPermissions = this.permissions;

      if ((!permission || Is.emptyObject(permission)) && !isUserRoot) {
         return false;
      }

      if (isUserRoot) {
         return true;
      }

      if (!userPermissions.length) {
         return false;
      }

      if (typeof permission === 'string') {
         permission = { key: permission };
      }

      if (
         !userPermissions.length ||
         (permission.adminScope === true && !userPermissions.includes(USER_PERMISSION_ADMIN_SCOPE)) ||
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

   toUserRefEntity(): UserRefEntity {
      return UserEntity.bindToClass(UserRefEntity, this);
   }
}

export class AuthTokenEntity {
   @Property()
   access: string;

   @Property()
   refresh: string;
}

export class AuthEntity extends BaseEntity {
   @Property()
   user: UserEntity;

   @Property({ swagger: { type: AuthTokenEntity } })
   tokens: AuthTokenEntity;
}

export class UserRefEntity extends IPickType(UserEntity, ['id', 'name', 'username', 'email', 'avatarUrl']) {}
