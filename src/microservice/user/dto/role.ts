import { IPartialType, IsIn, IsString } from '@shared-library';
import { $Enums } from '.prisma/user';
import { permissionKeys } from '@metadata';

export class CreateRoleDto {
   @IsString({ notEmpty: true, swagger: { description: 'The name of the role' } })
   name: string;

   @IsString({ optional: true, swagger: { description: 'The description of the role' } })
   description?: string;

   @IsIn(Object.values($Enums.AvailableStatus), { optional: true, swagger: { description: 'The status of the role' } })
   status?: $Enums.AvailableStatus;

   @IsIn(permissionKeys, { optional: true, each: 'unique', swagger: { description: 'The permissions of the role' } })
   permissions?: string[];
}

export class UpdateRoleDto extends IPartialType(CreateRoleDto) {}
