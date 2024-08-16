import { AvailableStatus } from '.prisma/order';
import { ClassRefSchema, Schema } from '@mvanvu/ujs';

export class CreateTableDto {
   @(Schema.mongoId().decorate())
   restaurantId: string;

   @(Schema.enum(AvailableStatus).optional().decorate())
   status?: AvailableStatus;

   @(Schema.uint(true).decorate())
   number: number;

   @(Schema.content().decorate())
   area?: string;
}

export class UpdateTableDto extends ClassRefSchema.Partial(CreateTableDto) {}
