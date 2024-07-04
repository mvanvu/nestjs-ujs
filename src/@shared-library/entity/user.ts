import { Is } from '@mvanvu/ujs';
import { UserStatus } from '.prisma/user';
import { BaseEntity } from './base';
import { PermissionOptions } from '../type/common';
import { IPickType } from './mapped-type';
import { GroupEntity } from './group';
import { USER_PERMISSION_ADMIN_SCOPE } from '@shared-library/constant/common';
import { appConfig } from '@metadata';
import { EnumSchema, ClassSchema, StringSchema, DateSchema } from '@shared-library/decorator';

export class UserGroupEntity extends IPickType(GroupEntity, ['id', 'name', 'groups', 'roles']) {}
export class UserEntity {
   @StringSchema()
   id: string;

   @EnumSchema(Object.values(UserStatus))
   status: UserStatus;

   @StringSchema()
   name?: string;

   @StringSchema()
   username?: string;

   @StringSchema()
   avatarUrl?: string;

   @StringSchema({ format: 'email' })
   email: string;

   @ClassSchema(UserGroupEntity)
   group?: UserGroupEntity;

   @DateSchema()
   createdAt: Date;

   @StringSchema()
   createdBy: string;

   @DateSchema()
   updatedAt: Date;

   @StringSchema()
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
      const rootUIDs = appConfig.get('rootUid').split(/\s*,+\s*/);

      if (rootUIDs.length) {
         for (const rootUid of rootUIDs) {
            if (
               (Is.string(rootUid, { format: 'email' }) && this.email === rootUid) ||
               this.id === rootUid ||
               (this.username && this.username === rootUid)
            ) {
               return true;
            }
         }
      }

      return false;
   }

   authorise(permission?: PermissionOptions): boolean {
      const isUserRoot = this.isRoot;
      const userPermissions = this.permissions;

      if ((!permission || Is.empty(permission)) && !isUserRoot) {
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
      return BaseEntity.bindToClass(this, UserRefEntity);
   }
}

export class AuthTokenEntity {
   @StringSchema()
   access: string;

   @StringSchema()
   refresh: string;
}

export class AuthEntity {
   @ClassSchema(UserEntity)
   user: UserEntity;

   @ClassSchema(AuthTokenEntity)
   tokens: AuthTokenEntity;
}

export class UserRefEntity extends IPickType(UserEntity, ['id', 'name', 'username', 'email', 'avatarUrl']) {}
