import { IPartialType, IProperty } from '@lib';
import { $Enums } from '.prisma/user';

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
}

export class UpdateRoleDto extends IPartialType(CreateRoleDto) {}

export class PermissionDto {
   @IProperty({
      swagger: 'The target model',
      transform: { fromType: 'string', toType: 'trim' },
      validate: [{ is: 'empty', not: true }],
   })
   refModel: string;

   @IProperty({ swagger: 'Can read', optional: true, validate: { is: 'boolean' } })
   canRead?: boolean;

   @IProperty({ swagger: 'Can create', optional: true, validate: { is: 'boolean' } })
   canCreate?: boolean;

   @IProperty({ swagger: 'Can update', optional: true, validate: { is: 'boolean' } })
   canUpdate?: boolean;

   @IProperty({ swagger: 'Can delete', optional: true, validate: { is: 'boolean' } })
   canDelete?: boolean;
}

export class PermissionListDto {
   @IProperty({
      swagger: {
         description: 'The permission list data',
         example: [{ refModel: 'user', canRead: true, canCreate: false, canUpdate: true, canDelete: false }],
         isArray: true,
      },
      validate: {
         is: 'array',
         meta: {
            notEmpty: true,
            rules: {
               refModel: 'string',
               canRead: 'boolean',
               canCreate: 'boolean',
               canUpdate: 'boolean',
               canDelete: 'boolean',
            },
         },
      },
   })
   permissions: PermissionDto[];
}
