import { IPartialType } from '@shared-library';
import { $Enums } from '.prisma/user';
import { Schema } from '@mvanvu/ujs';

export class CreateGroupDto {
   @Schema.content().decorate()
   name: string;

   @Schema.string().optional().decorate()
   description?: string;

   @Schema.enum($Enums.AvailableStatus).decorate()
   status?: $Enums.AvailableStatus;

   @Schema.mongoId().array().optional().decorate()
   groups?: string[];

   @Schema.mongoId().array().optional().decorate()
   roles?: string[];
}

export class UpdateGroupDto extends IPartialType(CreateGroupDto) {}
