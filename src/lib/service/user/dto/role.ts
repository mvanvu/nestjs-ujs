import { IPartialType, IProperty } from '@lib/common';
import { $Enums } from '.prisma/user';
import { permissionKeys } from '@metadata';

export class CreateRoleDto {
   @IProperty({
      transform: { fromType: 'string', toType: 'trim' },
      validate: [{ is: 'string' }, { is: 'empty', not: true }],
      swagger: { description: 'The name of the role' },
   })
   name: string;

   @IProperty({
      optional: true,
      transform: { fromType: 'string', toType: ['toStripTags', 'trim'] },
      validate: { is: 'string' },
      swagger: { description: 'The description of the role' },
   })
   description?: string;

   @IProperty({
      optional: true,
      validate: { is: 'inArray', meta: Object.values($Enums.AvailableStatus) },
      swagger: { description: 'The status of the role', enum: $Enums.AvailableStatus },
   })
   status?: $Enums.AvailableStatus;

   @IProperty({
      optional: true,
      validate: [{ is: 'arrayUnique' }, { is: 'inArray', each: true, meta: permissionKeys }],
      swagger: { description: 'The permissions of the role' },
   })
   permissions?: string[];
}

export class UpdateRoleDto extends IPartialType(CreateRoleDto) {}
