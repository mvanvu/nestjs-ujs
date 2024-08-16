import { UserSignUpDto } from './auth';
import { UserStatus } from '.prisma/user';
import { ClassRefSchema, Schema } from '@mvanvu/ujs';

export class CreateUserDto extends UserSignUpDto {
   @(Schema.enum(UserStatus).optional().decorate())
   status?: UserStatus;

   @(Schema.mongoId().optional().decorate())
   groupId?: string;

   @(Schema.imageUri().optional().decorate())
   avatarUrl?: string;
}

export class UpdateUserDto extends ClassRefSchema.Partial(CreateUserDto) {}
