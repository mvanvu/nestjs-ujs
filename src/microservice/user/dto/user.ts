import { EnumSchema, IDSchema, IPartialType, ImageSchema } from '@shared-library';
import { UserSignUpDto } from './auth';
import { UserStatus } from '.prisma/user';

export class CreateUserDto extends UserSignUpDto {
   @EnumSchema(Object.keys(UserStatus), { optional: true })
   status?: UserStatus;

   @IDSchema({ optional: true })
   groupId?: string;

   @ImageSchema({ optional: true })
   avatarUrl?: string;
}

export class UpdateUserDto extends IPartialType(CreateUserDto) {}
