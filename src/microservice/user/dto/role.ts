import { EnumSchema, IPartialType, StringSchema } from '@shared-library';
import { $Enums } from '.prisma/user';
import { permissionKeys } from '@metadata';

export class CreateRoleDto {
   @StringSchema({ notEmpty: true })
   name: string;

   @StringSchema({ optional: true })
   description?: string;

   @EnumSchema(Object.values($Enums.AvailableStatus), { optional: true })
   status?: $Enums.AvailableStatus;

   @EnumSchema(permissionKeys, { optional: true, each: 'unique' })
   permissions?: string[];
}

export class UpdateRoleDto extends IPartialType(CreateRoleDto) {}
