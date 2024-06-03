import { IPartialType, IProperty } from '@lib/common';
import { UserSignUpDto } from './auth';
import { UserStatus } from '.prisma/user';

export class CreateUserDto extends UserSignUpDto {
   @IProperty({
      optional: true,
      validate: { is: 'inArray', meta: Object.keys(UserStatus) },
      swagger: { description: 'The user status', enum: UserStatus },
   })
   status?: UserStatus;

   @IProperty({
      optional: true,
      validate: { is: 'mongoId' },
      swagger: { description: 'The group ID which assigned to the user' },
   })
   groupId?: string;
}

export class UpdateUserDto extends IPartialType(CreateUserDto) {}
