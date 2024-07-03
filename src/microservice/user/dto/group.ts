import { EnumSchema, IPartialType, StringSchema } from '@shared-library';
import { $Enums } from '.prisma/user';

export class CreateGroupDto {
   @StringSchema({ empty: false })
   name: string;

   @StringSchema({ optional: true })
   description?: string;

   @EnumSchema(Object.values($Enums.AvailableStatus), { optional: true })
   status?: $Enums.AvailableStatus;

   @StringSchema({ format: 'mongoId', isArray: 'unique', optional: true })
   groups?: string[];

   @StringSchema({ format: 'mongoId', isArray: 'unique', optional: true })
   roles?: string[];
}

export class UpdateGroupDto extends IPartialType(CreateGroupDto) {}
