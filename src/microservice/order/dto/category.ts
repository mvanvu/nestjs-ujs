import { AvailableStatus } from '.prisma/order';
import { ClassRefSchema, Schema } from '@mvanvu/ujs';

export class CreateCategoryDto {
   @(Schema.enum(AvailableStatus).decorate())
   status?: AvailableStatus;

   @(Schema.content().decorate())
   name: string;
}

export class UpdateCategoryDto extends ClassRefSchema.Partial(CreateCategoryDto) {}
