import { IPartialType, Property } from '@lib';
import { $Enums } from '.prisma/user';
import { permissionKeys } from '@metadata';

export class CreateRoleDto {
   @Property({
      transform: { fromType: 'string', toType: 'trim' },
      validate: [{ is: 'string' }, { is: 'empty', not: true }],
      swagger: { description: 'The name of the role' },
   })
   name: string;

   @Property({
      optional: true,
      transform: { fromType: 'string', toType: ['toStripTags', 'trim'] },
      validate: { is: 'string' },
      swagger: { description: 'The description of the role' },
   })
   description?: string;

   @Property({
      optional: true,
      validate: { is: 'inArray', meta: Object.values($Enums.AvailableStatus) },
      swagger: { description: 'The status of the role', enum: $Enums.AvailableStatus },
   })
   status?: $Enums.AvailableStatus;

   @Property({
      optional: true,
      validate: [{ is: 'arrayUnique' }, { is: 'inArray', each: true, meta: permissionKeys }],
      swagger: { description: 'The permissions of the role' },
   })
   permissions?: string[];
}

export class UpdateRoleDto extends IPartialType(CreateRoleDto) {}
