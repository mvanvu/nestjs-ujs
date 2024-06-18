import { IPickType, Property } from '@lib';

export class UserSignUpDto {
   @Property({
      validate: { is: 'string' },
      transform: { fromType: 'string', toType: 'trim' },
      optional: true,
      swagger: { description: 'The name of user', type: 'string', example: 'Rainy' },
   })
   name?: string;

   @Property({
      validate: { is: 'string' },
      optional: true,
      swagger: { description: 'The username of user', type: 'string', example: 'rainy.mi' },
   })
   username?: string;

   @Property({
      validate: { is: 'email' },
      swagger: { description: 'The email of user', type: 'string', example: 'rainy.mi@email.com' },
   })
   email: string;

   @Property({
      validate: [{ is: 'strongPassword', meta: { minLength: 8, noSpaces: true } }],
      swagger: { description: 'The password of user', type: 'string', example: 'MyStr0ngPassWord!' },
   })
   password: string;

   @Property({
      validate: { is: 'equals', meta: { equalsTo: 'password' } },
      swagger: { description: 'The confirm password', type: 'string', example: 'MyStr0ngPassWord!' },
   })
   password2: string;
}

export class UserSignInDto extends IPickType(UserSignUpDto, ['username', 'password']) {}

export class AuthTokenDto {
   @Property({
      validate: [{ is: 'string' }, { is: 'empty', not: true }],
      swagger: { description: 'The token to verify' },
   })
   token: string;
}

export class SendResetPasswordCodeDto extends IPickType(UserSignUpDto, ['email']) {}

export class VerifyAccountDto {
   @Property({
      validate: { is: 'matched', meta: /^[0-9a-fA-F]{24}:[a-zA-Z0-9-]+$/, code: 'VERIFY_CODE' },
      swagger: { description: 'The verification code to active the user account' },
   })
   code: string;
}

export class ResetPasswordDto extends IPickType(UserSignUpDto, ['password', 'password2']) {
   @Property({
      validate: { is: 'matched', meta: /^[0-9a-fA-F]{24}:[a-zA-Z0-9-]+$/, code: 'VERIFY_CODE' },
      swagger: { description: 'The verification code to verify to reset the new password' },
   })
   code: string;
}
