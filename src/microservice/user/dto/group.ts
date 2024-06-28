import { IPartialType, IsIn, IsMongoId, IsString } from '@shared-library';
import { $Enums } from '.prisma/user';

export class CreateGroupDto {
   @IsString({ notEmpty: true, swagger: { description: 'The name of the group' } })
   name: string;

   @IsString({ optional: true, swagger: { description: 'The description of the group' } })
   description?: string;

   @IsIn(Object.values($Enums.AvailableStatus), { optional: true, swagger: { description: 'The status of the group' } })
   status?: $Enums.AvailableStatus;

   @IsMongoId({ each: 'unique', optional: true, swagger: { description: 'The children of the group' } })
   groups?: string[];

   @IsMongoId({ each: 'unique', optional: true, swagger: { description: 'The roles of the group' } })
   roles?: string[];
}

export class UpdateGroupDto extends IPartialType(CreateGroupDto) {}
