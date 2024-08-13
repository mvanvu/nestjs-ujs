import { IPartialType } from '@shared-library';
import { AvailableStatus } from '.prisma/content';
import { Schema } from '@mvanvu/ujs';

export class CreateTagDto {
   @Schema.enum(AvailableStatus).optional().decorate()
   status?: AvailableStatus;

   @Schema.content().decorate()
   name: string;
}

export class UpdateTagDto extends IPartialType(CreateTagDto) {}
