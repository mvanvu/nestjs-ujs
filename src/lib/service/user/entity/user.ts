import { appConfig } from '@metadata';
import { Is } from '@mvanvu/ujs';
import { UserStatus } from '.prisma/user';
import { IProperty } from '@lib/common/decorator/property';
import { BaseEntity } from '@lib/common/entity/base';
import { PermissionOptions } from '@lib/common/type/common';
import { IPickType } from '@lib/common';
import { GroupEntity } from './group';

export class UserGroupEntity extends IPickType(GroupEntity, ['id', 'name', 'groups', 'roles']) {}

export class UserEntity extends BaseEntity {
   @IProperty()
   id: string;

   @IProperty({ swagger: { enum: UserStatus } })
   status: UserStatus;

   @IProperty()
   name?: string;

   @IProperty()
   username?: string;

   @IProperty()
   email: string;

   @IProperty({ swagger: { type: UserGroupEntity } })
   group: UserGroupEntity;

   @IProperty()
   createdAt: Date;

   @IProperty()
   createdBy: string;

   @IProperty()
   updatedAt: Date;

   @IProperty()
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
