import { IPartialType, IProperty } from '@lib';
import { $Enums } from '.prisma/user';
import { permissionKeys } from '@lib/service';

export class CreateRoleDto {
   @IProperty({
      swagger: 'The name of the role',
      transform: { fromType: 'string', toType: 'trim' },
      validate: [{ is: 'string' }, { is: 'empty', not: true }],
   })
   name: string;

   @IProperty({
      swagger: 'The description of the role',
      optional: true,
      transform: { fromType: 'string', toType: ['toStripTags', 'trim'] },
      validate: { is: 'string' },
   })
   description?: string;

   @IProperty({
      swagger: { description: 'The status of the role', example: 'ACTIVE' },
      optional: true,
      validate: { is: 'string' },
   })
   status?: $Enums.RoleStatus;

   @IProperty({
      swagger: { description: 'The permissions of the role', example: permissionKeys },
      optional: true,
      validate: { is: 'inArray', meta: permissionKeys },
   })
   permissions?: string[];
}

export class UpdateRoleDto extends IPartialType(CreateRoleDto) {}
