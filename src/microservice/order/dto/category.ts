import { AvailableStatus } from '.prisma/order';
import { Schema } from '@mvanvu/ujs';
import { IPartialType } from '@shared-library';

export class CreateCategoryDto {
   @Schema.enum(AvailableStatus).decorate()
   status?: AvailableStatus;

   @Schema.content().decorate()
   name: string;
}

export class UpdateCategoryDto extends IPartialType(CreateCategoryDto) {}
