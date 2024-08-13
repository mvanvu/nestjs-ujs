import { AvailableStatus } from '.prisma/order';
import { Schema } from '@mvanvu/ujs';
import { IPartialType } from '@shared-library';

export class CreateTableDto {
   @Schema.mongoId().decorate()
   restaurantId: string;

   @Schema.enum(AvailableStatus).optional().decorate()
   status?: AvailableStatus;

   @Schema.uint(true).decorate()
   number: number;

   @Schema.content().decorate()
   area?: string;
}

export class UpdateTableDto extends IPartialType(CreateTableDto) {}
