import { IPartialType, IsIn, IsMongoId, IsString } from '@shared-library';
import { UserSignUpDto } from './auth';
import { UserStatus } from '.prisma/user';

export class CreateUserDto extends UserSignUpDto {
   @IsIn(Object.keys(UserStatus), { optional: true, swagger: { description: 'The user status' } })
   status?: UserStatus;

   @IsMongoId({ optional: true, swagger: { description: 'The group ID which assigned to the user' } })
   groupId?: string;

   @IsString({ optional: true, url: true, swagger: { description: 'The avatar image URL of the user' } })
   avatarUrl?: string;
}

export class UpdateUserDto extends IPartialType(CreateUserDto) {}
