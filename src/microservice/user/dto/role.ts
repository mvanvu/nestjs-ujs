import { EnumSchema, IPartialType, NameSchema, StringSchema } from '@shared-library';
import { $Enums } from '.prisma/user';
import { getPermissionKeys } from '@metadata';
const permissionKeys = getPermissionKeys();

export class CreateRoleDto {
   @NameSchema()
   name: string;

   @StringSchema({ optional: true })
   description?: string;

   @EnumSchema(Object.values($Enums.AvailableStatus), { optional: true })
   status?: $Enums.AvailableStatus;

   @EnumSchema(permissionKeys, { optional: true, isArray: 'unique' })
   permissions?: string[];
}

export class UpdateRoleDto extends IPartialType(CreateRoleDto) {}
