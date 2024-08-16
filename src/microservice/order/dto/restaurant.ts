import { AvailableStatus } from '.prisma/content';
import { ClassRefSchema, Schema } from '@mvanvu/ujs';
import { UserRefEntity } from '@shared-library';

export class CreateRestaurantDto {
   @(Schema.mongoId().decorate())
   ownerId: string;

   @(Schema.classRef(UserRefEntity).decorate())
   owner: UserRefEntity;

   @(Schema.enum(AvailableStatus).optional().decorate())
   status?: AvailableStatus;

   @(Schema.content().decorate())
   name: string;

   @(Schema.safeHTML().optional().decorate())
   description?: string;

   @(Schema.imageUri().optional().decorate())
   imageUrl?: string;

   @(Schema.content().optional().decorate())
   address?: string;

   @(Schema.content().optional().decorate())
   phoneNumber?: string;

   @(Schema.email().optional().decorate())
   email?: string;
}

export class UpdateRestaurantDto extends ClassRefSchema.Partial(CreateRestaurantDto) {}
