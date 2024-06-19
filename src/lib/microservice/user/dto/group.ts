import { IPartialType, Property } from '@lib/common';
import { $Enums } from '.prisma/user';

export class CreateGroupDto {
   @Property({
      transform: { fromType: 'string', toType: 'trim' },
      validate: [{ is: 'string' }, { is: 'empty', not: true }],
      swagger: { description: 'The name of the group' },
   })
   name: string;

   @Property({
      optional: true,
      transform: { fromType: 'string', toType: ['toStripTags', 'trim'] },
      validate: { is: 'string' },
      swagger: { description: 'The description of the group' },
   })
   description?: string;

   @Property({
      optional: true,
      validate: { is: 'inArray', meta: Object.values($Enums.AvailableStatus) },
      swagger: { description: 'The status of the group', enum: $Enums.AvailableStatus },
   })
   status?: $Enums.AvailableStatus;

   @Property({
      optional: true,
      validate: [{ is: 'mongoId', each: true }, { is: 'arrayUnique' }],
      swagger: { description: 'The children of the group' },
   })
   groups?: string[];

   @Property({
      optional: true,
      validate: [{ is: 'mongoId', each: true }, { is: 'arrayUnique' }],
      swagger: { description: 'The children of the group' },
   })
   roles?: string[];
}

export class UpdateGroupDto extends IPartialType(CreateGroupDto) {}
