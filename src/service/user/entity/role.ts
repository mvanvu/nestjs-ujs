import { BaseEntity, IProperty, PaginationMeta } from '@lib';
import { $Enums } from '.prisma/user';

export class RoleEntity extends BaseEntity {
   @IProperty()
   id: string;

   @IProperty()
   root: boolean;

   @IProperty()
   name: string;

   @IProperty()
   status: $Enums.RoleStatus;

   @IProperty()
   createdAt?: Date;

   @IProperty()
   updatedAt?: Date;

   @IProperty()
   author?: { id: string; email: string; username: string };

   @IProperty()
   editor?: { id: string; email: string; username: string };

   @IProperty()
   permissions: { refModel: string; canRead: boolean; canCreate: boolean; canUpdate: boolean; canDelete: boolean }[];
}

export class SwaggerPaginationRoleEntity {
   @IProperty({ swagger: { isArray: true } })
   data: RoleEntity;

   @IProperty()
   meta: PaginationMeta;
}
