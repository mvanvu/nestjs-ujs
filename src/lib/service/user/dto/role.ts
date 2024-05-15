import { IPartialType, Property } from '@lib';
import { RoleStatus } from '.prisma/user';
import { permissionKeys } from '@lib/service';

export class CreateRoleDto {
   @Property({
      swagger: 'The name of the role',
      transform: { fromType: 'string', toType: 'trim' },
      validate: [{ is: 'string' }, { is: 'empty', not: true }],
   })
   name: string;

   @Property({
      swagger: 'The description of the role',
      optional: true,
      transform: { fromType: 'string', toType: ['toStripTags', 'trim'] },
      validate: { is: 'string' },
   })
   description?: string;

   @Property({
      swagger: { description: 'The status of the role', example: 'ACTIVE' },
      optional: true,
      validate: { is: 'string' },
   })
   status?: RoleStatus;

   @Property({
      swagger: { description: 'The permissions of the role', example: permissionKeys },
      optional: true,
      validate: { is: 'inArray', meta: permissionKeys },
   })
   permissions?: string[];
}

export class UpdateRoleDto extends IPartialType(CreateRoleDto) {}
