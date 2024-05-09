import { IPartialType, IProperty } from '@lib';
import { UserSignUpDto } from './auth';

export class CreateUserDto extends UserSignUpDto {
   @IProperty({
      swagger: { description: 'The role IDs which assigned to the user', isArray: true, type: 'string' },
      optional: true,
      validate: { is: 'string', each: true },
   })
   roles?: string[];
}

export class UpdateUserDto extends IPartialType(CreateUserDto) {}
