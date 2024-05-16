import { IPartialType, Property } from '@lib/common';
import { UserSignUpDto } from './auth';

export class CreateUserDto extends UserSignUpDto {
   @Property({
      swagger: { description: 'The role IDs which assigned to the user', isArray: true, type: 'string' },
      optional: true,
      validate: { is: 'string', each: true },
   })
   roles?: string[];
}

export class UpdateUserDto extends IPartialType(CreateUserDto) {}
