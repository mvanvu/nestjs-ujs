import { IPartialType, EntityProperty } from '@lib/common';
import { UserSignUpDto } from './auth';
import { ApiProperty } from '@nestjs/swagger';
import { UserStatus } from '.prisma/user';

export class CreateUserDto extends UserSignUpDto {
   @ApiProperty({ description: 'The user status', enum: UserStatus })
   @EntityProperty({ optional: true, validate: { is: 'inArray', meta: Object.keys(UserStatus) } })
   status?: UserStatus;

   @ApiProperty({ description: 'The role IDs which assigned to the user', isArray: true, type: 'string' })
   @EntityProperty({ optional: true, validate: { is: 'string', each: true } })
   roles?: string[];
}

export class UpdateUserDto extends IPartialType(CreateUserDto) {}
