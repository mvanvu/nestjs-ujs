import { IPartialType, IProperty } from '@lib/common';
import { $Enums } from '.prisma/user';

export class CreateGroupDto {
   @IProperty({
      transform: { fromType: 'string', toType: 'trim' },
      validate: [{ is: 'string' }, { is: 'empty', not: true }],
      swagger: { description: 'The name of the group' },
   })
   name: string;

   @IProperty({
      optional: true,
      transform: { fromType: 'string', toType: ['toStripTags', 'trim'] },
      validate: { is: 'string' },
      swagger: { description: 'The description of the group' },
   })
   description?: string;

   @IProperty({
      optional: true,
      validate: { is: 'inArray', meta: Object.values($Enums.AvailableStatus) },
      swagger: { description: 'The status of the group', enum: $Enums.AvailableStatus },
   })
   status?: $Enums.AvailableStatus;

   @IProperty({
      optional: true,
      validate: [{ is: 'mongoId', each: true }, { is: 'arrayUnique' }],
      swagger: { description: 'The children of the group' },
   })
   groups?: string[];

   @IProperty({
      optional: true,
      validate: [{ is: 'mongoId', each: true }, { is: 'arrayUnique' }],
      swagger: { description: 'The children of the group' },
   })
   roles?: string[];
}

export class UpdateGroupDto extends IPartialType(CreateGroupDto) {}
