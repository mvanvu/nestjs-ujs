import { $Enums } from '.prisma/user';
import { getPermissionKeys } from '@metadata';
import { ClassRefSchema, Schema } from '@mvanvu/ujs';
const permissionKeys = getPermissionKeys();

export class CreateRoleDto {
   @(Schema.content().decorate())
   name: string;

   @(Schema.string().optional().decorate())
   description?: string;

   @(Schema.enum($Enums.AvailableStatus).optional().decorate())
   status?: $Enums.AvailableStatus;

   @(Schema.enum(permissionKeys).array().optional().decorate())
   permissions?: string[];
}

export class UpdateRoleDto extends ClassRefSchema.Partial(CreateRoleDto) {}
