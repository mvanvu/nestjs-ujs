import { IPartialType, Property } from '@lib';
import { UserSignUpDto } from './auth';
import { UserStatus } from '.prisma/user';

export class CreateUserDto extends UserSignUpDto {
   @Property({
      optional: true,
      validate: { is: 'inArray', meta: Object.keys(UserStatus) },
      swagger: { description: 'The user status', enum: UserStatus },
   })
   status?: UserStatus;

   @Property({
      optional: true,
      validate: { is: 'mongoId' },
      swagger: { description: 'The group ID which assigned to the user' },
   })
   groupId?: string;

   @Property({
      optional: true,
      validate: { is: 'string' },
      swagger: { description: 'The avatar image URL of the user' },
   })
   avatarUrl?: string;
}

export class UpdateUserDto extends IPartialType(CreateUserDto) {}
