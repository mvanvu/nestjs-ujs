import { IPickType, PasswordSchema, StringSchema } from '@shared-library';

export class UserSignUpDto {
   @StringSchema({ optional: true, swagger: { description: 'The name of user', example: 'Rainy' } })
   name?: string;

   @StringSchema({ optional: true, swagger: { description: 'The username of user', example: 'rainy.mi' } })
   username?: string;

   @StringSchema({ format: 'email', swagger: { description: 'The email of user', example: 'rainy.mi@email.com' } })
   email: string;

   @PasswordSchema({
      minLength: 8,
      noSpaces: true,
      swagger: { description: 'The password of user', example: 'MyStr0ngPassWord!' },
   })
   password: string;

   @PasswordSchema({
      minLength: 8,
      noSpaces: true,
      equalsTo: 'password',
      swagger: { description: 'The password of user', example: 'MyStr0ngPassWord!' },
   })
   password2: string;
}

export class UserSignInDto extends IPickType(UserSignUpDto, ['username', 'password']) {}

export class AuthTokenDto {
   @StringSchema({ notEmpty: true })
   token: string;
}

export class SendResetPasswordCodeDto extends IPickType(UserSignUpDto, ['email']) {}

export class VerifyAccountDto {
   @StringSchema({ format: /^[0-9a-fA-F]{24}:[a-zA-Z0-9-]+$/ })
   code: string;
}

export class ResetPasswordDto extends IPickType(UserSignUpDto, ['password', 'password2']) {
   @StringSchema({ format: /^[0-9a-fA-F]{24}:[a-zA-Z0-9-]+$/ })
   code: string;
}
