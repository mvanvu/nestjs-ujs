import { IPartialType, EntityProperty } from '@lib/common';
import { UserSignUpDto } from './auth';
import { ApiProperty } from '@nestjs/swagger';

export class CreateUserDto extends UserSignUpDto {
   @ApiProperty({ description: 'The role IDs which assigned to the user', isArray: true, type: 'string' })
   @EntityProperty({ optional: true, validate: { is: 'string', each: true } })
   roles?: string[];
}

export class UpdateUserDto extends IPartialType(CreateUserDto) {}
