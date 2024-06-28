import { IPickType, IsEquals, IsString, IsStrongPassword } from '@shared-library';

export class UserSignUpDto {
   @IsString({ optional: true, swagger: { description: 'The name of user', example: 'Rainy' } })
   name?: string;

   @IsString({ optional: true, swagger: { description: 'The username of user', example: 'rainy.mi' } })
   username?: string;

   @IsString({ email: true, swagger: { description: 'The email of user', example: 'rainy.mi@email.com' } })
   email: string;

   @IsStrongPassword({
      minLength: 8,
      noSpaces: true,
      swagger: { description: 'The password of user', example: 'MyStr0ngPassWord!' },
   })
   password: string;

   @IsEquals('password', { swagger: { description: 'The confirm password', example: 'MyStr0ngPassWord!' } })
   password2: string;
}

export class UserSignInDto extends IPickType(UserSignUpDto, ['username', 'password']) {}

export class AuthTokenDto {
   @IsString({ notEmpty: true })
   token: string;
}

export class SendResetPasswordCodeDto extends IPickType(UserSignUpDto, ['email']) {}

export class VerifyAccountDto {
   @IsString({ regex: /^[0-9a-fA-F]{24}:[a-zA-Z0-9-]+$/ })
   code: string;
}

export class ResetPasswordDto extends IPickType(UserSignUpDto, ['password', 'password2']) {
   @IsString({ regex: /^[0-9a-fA-F]{24}:[a-zA-Z0-9-]+$/ })
   code: string;
}
