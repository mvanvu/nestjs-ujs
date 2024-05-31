import { IPartialType, EntityProperty } from '@lib/common';
import { $Enums } from '.prisma/user';
import { ApiProperty } from '@nestjs/swagger';

export class CreateGroupDto {
   @ApiProperty({ description: 'The name of the group' })
   @EntityProperty({
      transform: { fromType: 'string', toType: 'trim' },
      validate: [{ is: 'string' }, { is: 'empty', not: true }],
   })
   name: string;

   @ApiProperty({ description: 'The description of the group' })
   @EntityProperty({
      optional: true,
      transform: { fromType: 'string', toType: ['toStripTags', 'trim'] },
      validate: { is: 'string' },
   })
   description?: string;

   @ApiProperty({ description: 'The status of the group', enum: $Enums.AvailableStatus })
   @EntityProperty({ optional: true, validate: { is: 'inArray', meta: Object.values($Enums.AvailableStatus) } })
   status?: $Enums.AvailableStatus;

   @ApiProperty({ description: 'The permissions of the group' })
   @EntityProperty({ optional: true, validate: { is: 'inArray' } })
   permissions?: string[];
}

export class UpdateGroupDto extends IPartialType(CreateGroupDto) {}
