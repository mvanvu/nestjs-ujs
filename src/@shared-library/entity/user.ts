import { ClassRefSchema, Is } from '@mvanvu/ujs';
import { UserStatus } from '.prisma/user';
import { BaseEntity } from './base';
import { PermissionOptions } from '../type/common';
import { GroupEntity } from './group';
import { USER_PERMISSION_ADMIN_SCOPE } from '@shared-library/constant/common';
import { appConfig } from '@metadata';
import { Schema } from '@mvanvu/ujs';

export class UserGroupEntity extends ClassRefSchema.Pick(GroupEntity, ['id', 'name', 'groups', 'roles']) {}
export class UserEntity {
   @(Schema.mongoId().decorate())
   id: string;

   @(Schema.enum(Object.values(UserStatus)).decorate())
   status: UserStatus;

   @(Schema.content().optional().decorate())
   name?: string;

   @(Schema.content().optional().decorate())
   username?: string;

   @(Schema.imageUri().optional().decorate())
   avatarUrl?: string;

   @(Schema.email().decorate())
   email: string;

   @(Schema.classRef(UserGroupEntity).optional().decorate())
   group?: UserGroupEntity;

   @(Schema.dateTime().decorate())
   createdAt: Date;

   @(Schema.mongoId().optional().decorate())
   createdBy?: string;

   @(Schema.dateTime().optional().decorate())
   updatedAt?: Date;

   @(Schema.mongoId().optional().decorate())
   updatedBy?: string;

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
               (Schema.email().check(rootUid) && this.email === rootUid) ||
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
   @(Schema.jwt().decorate())
   access: string;

   @(Schema.jwt().decorate())
   refresh: string;
}

export class AuthEntity {
   @(Schema.classRef(UserEntity).decorate())

   // @ApiProperty({ type: UserEntity })
   user: UserEntity;

   @(Schema.classRef(AuthTokenEntity).decorate())
   tokens: AuthTokenEntity;
}

export class UserRefEntity extends ClassRefSchema.Pick(UserEntity, ['id', 'name', 'username', 'email', 'avatarUrl']) {}
