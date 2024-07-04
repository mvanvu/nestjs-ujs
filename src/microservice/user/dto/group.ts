import { EnumSchema, IDSchema, IPartialType, NameSchema, StringSchema } from '@shared-library';
import { $Enums } from '.prisma/user';

export class CreateGroupDto {
   @NameSchema()
   name: string;

   @StringSchema({ optional: true })
   description?: string;

   @EnumSchema(Object.values($Enums.AvailableStatus), { optional: true })
   status?: $Enums.AvailableStatus;

   @IDSchema({ isArray: 'unique', optional: true })
   groups?: string[];

   @IDSchema({ isArray: 'unique', optional: true })
   roles?: string[];
}

export class UpdateGroupDto extends IPartialType(CreateGroupDto) {}
