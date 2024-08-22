import { $Enums } from '.prisma/user';
import { ClassRefSchema, Schema } from '@mvanvu/ujs';

export class CreateGroupDto {
   @(Schema.content().decorate())
   name: string;

   @(Schema.string().optional().decorate())
   description?: string;

   @(Schema.enum($Enums.AvailableStatus).decorate())
   status?: $Enums.AvailableStatus;

   @(Schema.mongoId().array().optional().nullable(false).decorate())
   groups?: string[];

   @(Schema.mongoId().array().optional().nullable(false).decorate())
   roles?: string[];
}

export class UpdateGroupDto extends ClassRefSchema.Partial(CreateGroupDto) {}
