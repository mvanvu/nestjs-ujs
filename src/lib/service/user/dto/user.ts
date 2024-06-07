import { IPartialType, Property } from '@lib/common';
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
}

export class UpdateUserDto extends IPartialType(CreateUserDto) {}
