import { IPartialType, EntityProperty } from '@lib/common';
import { RoleStatus, UserStatus } from '.prisma/user';
import { ApiProperty } from '@nestjs/swagger';

export class CreateRoleDto {
   @ApiProperty({ description: 'The name of the role' })
   @EntityProperty({
      transform: { fromType: 'string', toType: 'trim' },
      validate: [{ is: 'string' }, { is: 'empty', not: true }],
   })
   name: string;

   @ApiProperty({ description: 'The description of the role' })
   @EntityProperty({
      optional: true,
      transform: { fromType: 'string', toType: ['toStripTags', 'trim'] },
      validate: { is: 'string' },
   })
   description?: string;

   @ApiProperty({ description: 'The status of the role', type: 'string', example: UserStatus.Active })
   @EntityProperty({ optional: true, validate: { is: 'string' } })
   status?: RoleStatus;

   @ApiProperty({ description: 'The permissions of the role' })
   @EntityProperty({ optional: true, validate: { is: 'inArray' } })
   permissions?: string[];
}

export class UpdateRoleDto extends IPartialType(CreateRoleDto) {}
